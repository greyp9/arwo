package io.github.greyp9.arwo.app.local.sh2.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.sh2.core.CommandLSH;
import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.locus.Locus;
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
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.mutex.CollectionU;
import io.github.greyp9.arwo.core.xed.action.XedActionCommand;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.op.OpUpdate;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyPageHtmlView;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

public class SHHandlerGet {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public SHHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doGet() throws IOException {
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        final AppTitle title = AppTitle.Factory.getResourceLabel(
                httpRequest, userState.getBundle(), Value.wrap("[", "]", httpRequest.getContextPath()));

        final Collection<CommandLSH> commands = CollectionU.copy(new ArrayList<>(), userState.getLSH().getCommands());
        new TextAreaLSH(httpRequest, userState).addTextArea(body);
        new TableLSH(commands, userState).addTable(body);
        final Preferences preferences = new Preferences(userState.getConfig());
        final String iconColor = Value.defaultOnEmpty(preferences.getIconColor(), "black");
        final String theme = Value.defaultOnEmpty(preferences.getTheme(), "default");
        new AppHtml(httpRequest).fixup(html, title, iconColor, theme);

        final byte[] entity = DocumentU.toXHtml(html);
        final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
        final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
        final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }

    private static final class TextAreaLSH {
        private final ServletHttpRequest httpRequest;
        private final AppUserState userState;

        private TextAreaLSH(final ServletHttpRequest httpRequest, final AppUserState userState) {
            this.httpRequest = httpRequest;
            this.userState = userState;
        }

        private void addTextArea(final Element html) throws IOException {
            final String command = "";
            final XedActionCommand action = new XedActionCommand(userState.getXedFactory());
            final Xed xedUI = action.getXedUI(userState.getLocale());
            final XedCursor cursor = new XedNav(xedUI).getRoot();
            final Bundle bundle = cursor.getXed().getBundle();
            final NameTypeValues ntv = NameTypeValuesU.create("command.commandType.command", command);
            final ValueInstance valueInstanceIn = ValueInstance.create(cursor.getTypeInstance(), ntv);
            new OpUpdate(null, action.getXed()).apply(cursor.getElement(), valueInstanceIn);

            final String qname = cursor.getTypeInstance().getQName().toString();
            final String submitID = userState.getSubmitID();
            final ActionFactory factory = new ActionFactory(submitID, bundle, App.Target.SESSION, qname, null);
            final Collection<String> collection = Collections.singleton(App.Action.COMMAND);
            final ActionButtons buttons = factory.create(App.Action.COMMAND, false, collection);
            final XedRequest xedRequest = new XedRequest(httpRequest, null, userState.getDocumentState());
            new PropertyPageHtmlView(new XedPropertyPageView(null, cursor, buttons), xedRequest).addContentTo(html);
        }
    }

    private static final class TableLSH {
        private final Collection<CommandLSH> commands;
        private final AppUserState userState;

        private TableLSH(final Collection<CommandLSH> commands, final AppUserState userState) {
            this.commands = commands;
            this.userState = userState;
        }

        public void addTable(final Element html) throws IOException {
            final RowSetMetaData metaData = createMetaData();
            final RowSet rowSet = createRowSet(metaData);
            final Bundle bundle = userState.getBundle();
            final Locus locus = userState.getLocus();
            final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
            final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
            TableU.addFooterStandard(table, bundle);
            final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
            final TableContext tableContext = new TableContext(
                    viewState, filter, userState.getSubmitID(), App.CSS.TABLE, bundle, userState.getLocus());
            final TableView tableView = new TableView(table, tableContext);
            tableView.addContentTo(html);

        }

        private RowSetMetaData createMetaData() {
            final ColumnMetaData[] columns = new ColumnMetaData[] {
                    new ColumnMetaData("dateSubmit", Types.TIMESTAMP, true),  // i18n metadata
                    new ColumnMetaData("in", Types.VARCHAR),  // i18n metadata
                    new ColumnMetaData("out", Types.VARCHAR),  // i18n metadata
                    new ColumnMetaData("dateFinish", Types.TIMESTAMP),  // i18n metadata
            };
            return new RowSetMetaData("lsh2HistoryType", columns);
        }

        private RowSet createRowSet(final RowSetMetaData metaData) {
            final RowSet rowSet = new RowSet(metaData, null, null);
            for (final CommandLSH commandSH : commands) {
                createRow(rowSet, commandSH);
            }
            return rowSet;
        }

        private void createRow(final RowSet rowSet, final CommandLSH command) {
            final InsertRow insertRow = new InsertRow(rowSet);
            insertRow.setNextColumn(command.getDate());
            insertRow.setNextColumn(command.getIn());
            insertRow.setNextColumn(command.getOut());
            insertRow.setNextColumn(command.getDateFinish());
            rowSet.add(insertRow.getRow());
        }
    }
}
