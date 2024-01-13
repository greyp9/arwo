package io.github.greyp9.arwo.app.jdbc.sh.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.subsystem.jdbc.SubsystemJDBC;
import io.github.greyp9.arwo.app.jdbc.sh.core.JDBCRequest;
import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.jdbc.query.History;
import io.github.greyp9.arwo.core.jdbc.query.Query;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.result.view.ResultsContext;
import io.github.greyp9.arwo.core.result.view.ResultsView;
import io.github.greyp9.arwo.core.util.CollectionU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedActionSQL;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.op.OpUpdate;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyPageHtmlView;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Properties;

@SuppressWarnings("PMD.ExcessiveImports")
public class JDBCCommandView extends JDBCView {
    private final SubsystemJDBC jdbc;

    public JDBCCommandView(final JDBCRequest request, final AppUserState userState) {
        super(request, userState);
        this.jdbc = userState.getJDBC();
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final JDBCRequest request = getRequest();
        final String queryID = request.getQueryID();
        final Query query = jdbc.getHistory().find(queryID);
        // if command id is not in the list of cached commands, redirect to clear command id from URL
        final boolean badReference = ((!Value.isEmpty(queryID)) && (query == null));
        return (badReference
                ? HttpResponseU.to302(PathU.toDir(request.getHttpRequest().getBaseURI(), request.getServer()))
                : addContentTo(html, query));
    }

    private HttpResponse addContentTo(final Element html, final Query query) throws IOException {
        final JDBCRequest request = getRequest();
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = request.getUserState();
        final Properties properties = jdbc.getProperties();
        // command input form (prep)
        final String sql = (query == null) ? properties.getProperty(App.Settings.SQL, "") : query.getText();
        properties.setProperty(App.Settings.SQL, sql);
        final XedActionSQL action = new XedActionSQL(userState.getXedFactory());
        final Xed xedUI = action.getXedUI(userState.getLocus().getLocale());
        final XedCursor cursor = new XedNav(xedUI).getRoot();
        final Bundle bundle = cursor.getXed().getBundle();
        final NameTypeValues ntv = NameTypeValuesU.create("sql.sqlType.sql", sql);
        final ValueInstance valueInstanceIn = ValueInstance.create(cursor.getTypeInstance(), ntv);
        new OpUpdate(null, action.getXed()).apply(cursor.getElement(), valueInstanceIn);
        // command input form
        final String qname = cursor.getTypeInstance().getQName().toString();
        final String submitID = userState.getSubmitID();
        final ActionFactory factory = new ActionFactory(submitID, bundle, App.Target.SESSION, qname, null);
        final ActionButtons buttons = factory.create(
                App.Action.SQL, false, CollectionU.toCollection(App.Action.SQL));
        final XedRequest xedRequest = new XedRequest(httpRequest, null, userState.getDocumentState());
        new PropertyPageHtmlView(new XedPropertyPageView(null, cursor, buttons), xedRequest).addContentTo(html);
        // contextual content
        if (query == null) {
            final History history = jdbc.getHistory();
            new JDBCHistoryView("jdbcHistoryType", history, bundle,  // i18n metadata
                    request, userState).addContentTo(html);
        } else {
            final ResultsContext context = userState.getResultsContext(httpRequest);
            new ResultsView(query.getResults(), context).addContentTo(html);
        }
        return null;
    }
}
