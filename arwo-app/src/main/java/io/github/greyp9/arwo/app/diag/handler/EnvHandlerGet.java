package io.github.greyp9.arwo.app.diag.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.menu2.core.MenuSession;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.menu2.view.MenuHtml;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.util.Collections;
import java.util.Map;
import java.util.Properties;

public class EnvHandlerGet {
    private final AppRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public EnvHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = userState.getAppRequest(httpRequest);
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doGet() throws IOException {
        // template html
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element header = new XPather(html, null).getElement(Html.XPath.HEADER);
        final Element content = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final Element footer = new XPather(html, null).getElement(Html.XPath.FOOTER);
        // context-specific content
        final AppRequest requestOp = userState.getAppRequest(httpRequest);
        final AppTitle title = AppTitle.Factory.getHostLabel(httpRequest, requestOp.getBundle());
        addMenus(header);
        // content
        addContentReqHdr(content);
        addContentSysProps(content);
        addContentSysEnv(content);
        return new AppHtmlView(httpRequest, userState, title, null, null)
                .title(header)
                .alerts(header)
                .actionLocale(header)
                .statusBar(footer)
                .appHtml(html)
                .toHttpResponse(html);
    }

    private void addMenus(final Element header) {
        final MenuItem menu = new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU2, MENU_KEY, null,
                new MenuSession().toMenuItem(PathU.toPath(MENU_KEY, App.Target.SESSION)))
                .applyFrom(userState.getMenuSystemState());
        new MenuHtml(httpRequest, userState.getBundle(), userState.getSubmitID(), STYLE_HOME)
                .addTo(header, true, "m", Collections.singletonList(menu));
    }

    private static final String MENU_KEY = "/menu2/env";
    private static final String STYLE_HOME = "background-color: brown; color: white;";

    private RowSetMetaData createMetaData(final String tableId) {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(Html.NAME, Types.VARCHAR, true),
                new ColumnMetaData(Html.VALUE, Types.VARCHAR),
        };
        return new RowSetMetaData(tableId, columns);
    }

    private void addContentReqHdr(final Element html) throws IOException {
        final RowSetMetaData metaData = createMetaData("env.reqhdrType");
        final RowSet rowSet = new RowSet(metaData, null, null);
        final NameTypeValues headers = httpRequest.getHttpRequest().getHeaders();
        for (final NameTypeValue header : headers) {
            final InsertRow insertRow = new InsertRow(rowSet);
            insertRow.setNextColumn(header.getName());
            insertRow.setNextColumn(header.getValue());
            rowSet.add(insertRow.getRow());
        }
        addContent(html, rowSet);
    }

    private void addContentSysProps(final Element html) throws IOException {
        final RowSetMetaData metaData = createMetaData("env.syspropsType");
        final RowSet rowSet = new RowSet(metaData, null, null);
        final Properties properties = System.getProperties();
        for (Map.Entry<Object, Object> property : properties.entrySet()) {
            final InsertRow insertRow = new InsertRow(rowSet);
            insertRow.setNextColumn(property.getKey());
            insertRow.setNextColumn(property.getValue());
            rowSet.add(insertRow.getRow());
        }
        addContent(html, rowSet);
    }

    private void addContentSysEnv(final Element html) throws IOException {
        final RowSetMetaData metaData = createMetaData("env.sysenvType");
        final RowSet rowSet = new RowSet(metaData, null, null);
        final Map<String, String> environment = System.getenv();
        for (final Map.Entry<String, String> entry : environment.entrySet()) {
            final InsertRow insertRow = new InsertRow(rowSet);
            insertRow.setNextColumn(entry.getKey());
            insertRow.setNextColumn(entry.getValue());
            rowSet.add(insertRow.getRow());
        }
        addContent(html, rowSet);
    }

    private void addContent(final Element html, final RowSet rowSet) throws IOException {
        final Bundle bundle = request.getBundle();
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(rowSet.getMetaData(), bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
        final TableContext tableContext = new TableContext(
                viewState, filter, userState.getSubmitID(), App.CSS.TABLE, bundle, userState.getLocus());
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
    }
}
