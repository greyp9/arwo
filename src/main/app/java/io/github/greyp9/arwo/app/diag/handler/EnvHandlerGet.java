package io.github.greyp9.arwo.app.diag.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.menu.view.MenuView;
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
import io.github.greyp9.arwo.core.view.StatusBarView;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.sql.Types;
import java.util.Locale;
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
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        // context-specific content
        final AppRequest request = userState.getAppRequest(httpRequest);
        final AppTitle title = AppTitle.Factory.getHostLabel(httpRequest, request.getBundle());
        addHeaderView(body, title);
        // content
        addContentReqHdr(body);
        addContentSysProps(body);
        addContentSysEnv(body);
        // touch ups
        final boolean displayAlerts = (httpRequest.getHttpRequest().getHeader(App.Header.RESULT) == null);
        new AlertsView(displayAlerts, userState.getAlerts(), userState.getLocus(), userState.getBundle(),
                userState.getSubmitID()).addContentTo(body);
        new StatusBarView(httpRequest, userState.getLocus()).addContentTo(body);
        new AppHtml(httpRequest).fixup(html, title);
        // package into response
        final byte[] entity = DocumentU.toXHtml(html);
        final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
        final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
        final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }

    private void addHeaderView(final Element html, final AppTitle title) throws IOException {
        // context menu
        final MenuView menuView = new MenuView(request.getBundle(), httpRequest, userState.getMenuSystem());
        menuView.addContentTo(html, AppMenuFactory.Const.DASHBOARD, true);
        // context title (+ text filters)
        final Element divMenus = menuView.addTitle(html, title);
        divMenus.getClass();
        // settings property strips
        final Locale locale = userState.getLocus().getLocale();
        final String submitID = userState.getSubmitID();
        final Properties properties = userState.getProperties();
        new XedActionLocale(userState.getXedFactory(), locale).addContentTo(html, submitID, properties);
    }

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
