package io.github.greyp9.arwo.app.apptest.fs.servlet;

import io.github.greyp9.arwo.app.core.servlet.ServletU;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.gz.HttpResponseGZipU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.submit.SubmitTokenU;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.sql.Types;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

@SuppressWarnings({ "PMD.ExcessiveImports", "PMD.TooManyMethods" })
public class FileSystemServlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = -3409881413109441597L;

    private transient AppUserState state;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        super.init(config);
        synchronized (this) {
            final String submitID = Integer.toHexString(hashCode());
            final DateX dateX = new DateX(HttpDateU.Const.DEFAULT, TimeZone.getTimeZone("UTC"));
            final Locus locus = new Locus(Locale.getDefault(), dateX);
            state = new AppUserState(submitID, new ViewStates(), locus);
        }
    }

    @Override
    public final void destroy() {
        synchronized (this) {
            this.state = null;
        }
    }

    @Override
    protected final void doGet(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        // get request context
        final ServletHttpRequest httpRequest = ServletU.read(request);
        // process request
        AppUserState stateRequest;
        synchronized (this) {
            stateRequest = state;
        }
        final HttpResponse httpResponse = new FileSystemHandlerGet(httpRequest, stateRequest).doGet();
        final HttpResponse httpResponseGZ = HttpResponseGZipU.toHttpResponseGZip(httpRequest, httpResponse);
        ServletU.write(httpResponseGZ, response);
    }

    @Override
    protected final void doPost(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        // get request context
        final ServletHttpRequest httpRequest = ServletU.read(request);
        // process request
        AppUserState stateRequest;
        synchronized (this) {
            stateRequest = state;
        }
        final HttpResponse httpResponse = new FileSystemHandlerPost(httpRequest, stateRequest).doPost();
        ServletU.write(httpResponse, response);
    }

    public static class FileSystemHandlerGet {
        private final ServletHttpRequest httpRequest;
        private final AppUserState userState;

        public FileSystemHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
            this.httpRequest = httpRequest;
            this.userState = userState;
        }

        @SuppressWarnings("PMD.ConfusingTernary")
        public final HttpResponse doGet() throws IOException {
            HttpResponse httpResponse;
            final String pathInfo = httpRequest.getPathInfo();
            final String query = httpRequest.getQuery();
            if (pathInfo == null) {
                httpResponse = HttpResponseU.toHttpResponse302(PathU.toDir(httpRequest.getBaseURI()));
            } else if (!pathInfo.endsWith(Http.Token.SLASH)) {
                httpResponse = HttpResponseU.toHttpResponse302(PathU.toDir(httpRequest.getURI()));
            } else if (query != null) {
                httpResponse = HttpResponseU.toHttpResponse302(httpRequest.getURI());
            } else {
                httpResponse = doGetHtml(pathInfo);
            }
            return httpResponse;
        }

        private HttpResponse doGetHtml(final String pathInfo) throws IOException {
            // template html
            final Document html = DocumentU.toDocument(StreamU.read(ResourceU.resolve(Const.HTML)));
            final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
            // cursor content
            addContentTo(pathInfo, body);
            // touch ups
            new AppHtml(httpRequest).fixup(html);
            // package into response
            final byte[] entity = DocumentU.toXHtml(html);
            final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
            final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
            final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
            return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
        }

        private void addContentTo(final String pathInfo, final Element html) throws IOException {
            final File folder = new File(SystemU.userHome(), pathInfo);
            final RowSetMetaData metaData = createMetaData();
            final RowSet rowSet = loadRowSet(folder, metaData);
            final Bundle bundle = new Bundle();
            final Locus locus = userState.getLocus();
            final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
            final String submitID = userState.getSubmitID();
            final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
            final TableContext tableContext = new TableContext(viewState, submitID, "table", bundle, locus);
            // render
            final TableView tableView = new TableView(table, tableContext);
            tableView.addContentTo(html);
        }

        public static RowSetMetaData createMetaData() {
            final ColumnMetaData[] columns = new ColumnMetaData[] {
                    new ColumnMetaData("type", Types.VARCHAR),
                    new ColumnMetaData("name", Types.VARCHAR, true),
                    new ColumnMetaData("mtime", Types.TIMESTAMP),
                    new ColumnMetaData("ext", Types.VARCHAR),
                    new ColumnMetaData("size", Types.BIGINT),
            };
            return new RowSetMetaData("lfsFolderType", columns);
        }

        public static RowSet loadRowSet(final File folder, final RowSetMetaData metaData) throws IOException {
            final RowSet rowSet = new RowSet(metaData, null, null);
            final File[] files = FileU.listFiles(folder);
            for (final File file : files) {
                loadRow(rowSet, file);
            }
            return rowSet;
        }

        public static void loadRow(final RowSet rowSet, final File file) throws UnsupportedEncodingException {
            final boolean isDirectory = file.isDirectory();
            final String extension = (isDirectory ? null : new FileX(file.getName()).getExtension());
            final InsertRow insertRow = new InsertRow(rowSet);
            insertRow.setNextColumn(null);
            insertRow.setNextColumn(file.getName());
            insertRow.setNextColumn(new Date(file.lastModified()));
            insertRow.setNextColumn(extension);
            insertRow.setNextColumn(file.length());
            rowSet.add(insertRow.getRow());
        }

        private static class Const {
            private static final String HTML = "io/github/greyp9/arwo/html/xed/xed.html";
        }
    }

    public static class FileSystemHandlerPost {
        private final ServletHttpRequest httpRequest;
        private final AppUserState userState;

        public FileSystemHandlerPost(final ServletHttpRequest httpRequest, final AppUserState userState) {
            this.httpRequest = httpRequest;
            this.userState = userState;
        }

        public final HttpResponse doPost() throws IOException {
            final byte[] entity = StreamU.read(httpRequest.getHttpRequest().getEntity());
            final NameTypeValues nameTypeValues = HttpArguments.toArguments(entity);
            final String submitID = userState.getSubmitID();
            HttpResponse httpResponse = HttpResponseU.toHttpResponse302(httpRequest.getURI());
            for (final NameTypeValue nameTypeValue : nameTypeValues) {
                if (submitID.equals(nameTypeValue.getName())) {
                    final SubmitToken token = SubmitTokenU.fromString(nameTypeValue.getValueS());
                    if (token != null) {
                        httpResponse = apply(token, nameTypeValues, httpResponse);
                    }
                }
            }
            return httpResponse;
        }

        private HttpResponse apply(final SubmitToken token, final NameTypeValues nameTypeValues,
                                   final HttpResponse httpResponseIn) throws IOException {
            //HttpResponse httpResponse = httpResponseIn;
            final String subject = token.getSubject();
            if (App.Target.VIEW_STATE.equals(subject)) {
                userState.getViewStates().apply(token, nameTypeValues);
            }
            return httpResponseIn;
        }
    }
}
