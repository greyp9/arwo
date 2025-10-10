package io.github.greyp9.arwo.s3.handler;

import io.github.greyp9.arwo.app.cache.core.Cache;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.cache.CacheRowSetSource;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.config.CursorS3;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.MenuContext;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import io.github.greyp9.arwo.core.text.TextU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.s3.connection.S3ConnectionFactory;
import io.github.greyp9.arwo.s3.connection.S3ConnectionResource;
import io.github.greyp9.arwo.s3.data.S3RowSetSource;
import io.github.greyp9.arwo.s3.view.EndpointRowSet;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import software.amazon.awssdk.core.ResponseBytes;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.GetObjectRequest;
import software.amazon.awssdk.services.s3.model.GetObjectResponse;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

public class S3HandlerGet {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    //private final Properties properties;

    public S3HandlerGet(final ServletHttpRequest httpRequest,
                        final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        //this.properties = properties;
    }

    public final HttpResponse doGet() throws IOException {
        HttpResponse httpResponse;
        try {
            httpResponse = doGet2();
        } catch (IOException e) {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
            httpResponse = HttpResponseU.to500(e.getMessage());
        }
        return httpResponse;
    }

    private HttpResponse doGet2() throws IOException {
        final HttpResponse httpResponse;
        final String pathInfo = httpRequest.getPathInfo();
        final boolean isFolder = pathInfo.endsWith(Http.Token.SLASH);
        final Pather pather = new Pather(pathInfo);
        final String endpoint = pather.getLeftToken();
        final String resource = pather.getRight();
        if (Value.isEmpty(endpoint)) {
            httpResponse = doGetInventory();
        } else if (resource == null) {
            httpResponse = HttpResponseU.to302(httpRequest.getBaseURI() + pather.getLeft() + Http.Token.SLASH);
        } else if (isFolder) {
            httpResponse = doGetFolder(endpoint, resource.substring(Http.Token.SLASH.length()));
        } else {
            httpResponse = doGetFile(endpoint, resource.substring(Http.Token.SLASH.length()));
        }
        return httpResponse;
    }

    private HttpResponse doGetInventory() throws IOException {
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element body = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final UserStateTable table = new UserStateTable(userState, httpRequest.getPathInfo(), httpRequest.getDate());
        table.toTableView(new EndpointRowSet(httpRequest, userState).getRowSet()).addContentTo(body);
        final String labelContext = TextU.wrapBracket(httpRequest.getPathInfo());
        final NameTypeValue facetCacheEnabled = new NameTypeValue(
                App.Action.CACHE, PropertiesU.isBoolean(userState.getProperties(), App.Action.CACHE));
        final AppTitle appTitle = AppTitle.Factory.getResourceLabel(httpRequest, userState.getBundle(),
                labelContext, facetCacheEnabled.toStringNV());
        final MenuSystem menuSystem = userState.getMenuSystem();
        final List<MenuItem> menuItems = Collections.singletonList(
                menuSystem.get(httpRequest.getServletPath(), AppMenuFactory.Const.DASHBOARD)
        );
        final MenuContext menuContext = new MenuContext(menuSystem, menuItems);
        return new AppHtmlView(httpRequest, userState, appTitle, menuContext, App.Token.EMPTY).fixup(html);
    }

    private HttpResponse doGetFile(final String endpoint, final String path) throws IOException {
        final Xed xed = userState.getDocumentState().getSession(App.Servlet.SETTINGS).getXed();
        final CursorS3 cursorS3 = new CursorS3(xed, endpoint);
        final String regionName = cursorS3.getRegion();
        final String bucketName = cursorS3.getBucket();
        // get remote connection
        final ConnectionCache cache = userState.getS3().getCache();
        final String connectionName = Value.join(Http.Token.SLASH, regionName, bucketName);
        final S3ConnectionFactory connectionFactory = new S3ConnectionFactory(regionName, bucketName);
        final S3ConnectionResource resource =
                (S3ConnectionResource) cache.getResource(connectionName, connectionFactory);
        // get remote resource
        final S3Client s3Client = resource.getConnection().getS3Client();
        final GetObjectRequest getObjectRequest = GetObjectRequest.builder()
                .bucket(bucketName).key(path).build();
        final ResponseBytes<GetObjectResponse> objectAsBytes = s3Client.getObjectAsBytes(getObjectRequest);
        // prep resource for inclusion in response payload
        final byte[] payload = objectAsBytes.asByteArray();
        final long lastModified = objectAsBytes.response().lastModified().toEpochMilli();
        final String baseURI = httpRequest.getHttpRequest().getResource();
        final FileMetaData metaData = new FileMetaData(baseURI, payload.length, lastModified, 0L, false);
        final MetaFile metaFile = toMetaFile(metaData, new ByteArrayInputStream(payload));
        // optionally cache response payload
        final HttpResponse httpResponse;
        final boolean cacheItem = PropertiesU.isBoolean(userState.getProperties(), App.Action.CACHE);
        if (cacheItem) {
            final ResourceCache resourceCache = userState.getCache();
            final String id = Http.Token.SLASH + UUID.nameUUIDFromBytes(UTF8Codec.toBytes(baseURI));
            metaData.getProperties().setProperty(Html.HREF, baseURI + Http.Token.QUERY + App.Action.CACHE);
            resourceCache.putFile(id, metaFile);
            httpResponse = HttpResponseU.to302(PathU.toPath(resourceCache.getEndpoint(), Cache.CONTEXT_METAFILES, id));
        } else {
            httpResponse = HttpResponseU.to200(metaFile);
        }
        return httpResponse;
    }

    private MetaFile toMetaFile(final FileMetaData metaData, final ByteArrayInputStream is) throws IOException {
        final String tgzExtension = ".tar.gz";
        final String gzipExtension = ".gz";
        final String path = metaData.getPath();
        final boolean isGZ = (path.endsWith(gzipExtension) && !path.endsWith(tgzExtension));
        final String contentEncoding = isGZ ? Http.Header.GZIP : null;
        final int endIndex = isGZ ? (path.length() - gzipExtension.length()) : path.length();
        final String pathEffective = path.substring(0, endIndex);
        final Preferences preferences = new Preferences(userState.getConfig());
        final String contentType = Value.defaultOnEmpty(
                preferences.getMIMETypeIt(pathEffective), Http.Mime.TEXT_PLAIN_UTF8);
        return new MetaFile(metaData, contentType, contentEncoding, is);
    }

    private HttpResponse doGetFolder(final String endpoint, final String path) throws IOException {
        final Xed xed = userState.getDocumentState().getSession(App.Servlet.SETTINGS).getXed();
        final CursorS3 cursorS3 = new CursorS3(xed, endpoint);
        final String regionName = cursorS3.getRegion();
        final String bucketName = cursorS3.getBucket();
        // get remote connection
        final ConnectionCache connectionCache = userState.getS3().getCache();
        final String connectionName = Value.join(Http.Token.SLASH, regionName, bucketName);
        final S3ConnectionFactory connectionFactory = new S3ConnectionFactory(regionName, bucketName);
        final S3ConnectionResource resource =
                (S3ConnectionResource) connectionCache.getResource(connectionName, connectionFactory);
        // get remote resource (or cached copy, if present)
        final ResourceCache resourceCache = userState.getCache();
        final RowSetSource rowSetSourceS3 = new S3RowSetSource(
                resource.getConnection(), httpRequest.getBaseURI(), bucketName, path);
        final RowSetSource rowSetSource = new CacheRowSetSource(
                resourceCache, rowSetSourceS3, httpRequest.getHttpRequest().getResource());
        final RowSet rowSet;
        try {
            rowSet = rowSetSource.getRowSet();
        } catch (Exception e) {
            throw new IOException(e);
        }
        // resourceCache.clear();  // debugging aid
        // render resource
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element body = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final UserStateTable table = new UserStateTable(userState, httpRequest.getPathInfo(), httpRequest.getDate());
        table.toTableView(rowSet).addContentTo(body);
        final String labelContext = TextU.wrapBracket(httpRequest.getPathInfo());
        final NameTypeValue facetCacheEnabled = new NameTypeValue(
                App.Action.CACHE, PropertiesU.isBoolean(userState.getProperties(), App.Action.CACHE));
        final AppTitle appTitle = AppTitle.Factory.getResourceLabel(httpRequest, userState.getBundle(),
                labelContext, regionName, bucketName, facetCacheEnabled.toStringNV());
        final MenuSystem menuSystem = userState.getMenuSystem();
        final List<MenuItem> menuItems = Collections.singletonList(
                menuSystem.get(httpRequest.getServletPath(), AppMenuFactory.Const.DASHBOARD)
        );
        final MenuContext menuContext = new MenuContext(menuSystem, menuItems);
        return new AppHtmlView(httpRequest, userState, appTitle, menuContext, App.Token.EMPTY).fixup(html);
    }
}
