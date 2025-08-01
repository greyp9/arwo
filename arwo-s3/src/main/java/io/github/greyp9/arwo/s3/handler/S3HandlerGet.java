package io.github.greyp9.arwo.s3.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.cache.CacheRowSetSource;
import io.github.greyp9.arwo.core.cache.ResourceCache;
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
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.s3.connection.S3ConnectionFactory;
import io.github.greyp9.arwo.s3.connection.S3ConnectionResource;
import io.github.greyp9.arwo.s3.data.S3RowSetSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import software.amazon.awssdk.core.ResponseBytes;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.GetObjectRequest;
import software.amazon.awssdk.services.s3.model.GetObjectResponse;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.Properties;

public class S3HandlerGet {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Properties properties;

    public S3HandlerGet(final ServletHttpRequest httpRequest,
                        final AppUserState userState,
                        final Properties properties) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.properties = properties;
    }

    public final HttpResponse doGet() throws IOException {
        final HttpResponse httpResponse;
        final String pathInfo = httpRequest.getPathInfo();
        if (pathInfo.endsWith("/")) {
            httpResponse = doGetFolder(pathInfo.substring("/".length()));
        } else {
            httpResponse = doGetFile(pathInfo.substring("/".length()));
        }
        return httpResponse;
    }

    private HttpResponse doGetFile(final String prefix) throws IOException {
        final String bucketName = properties.getProperty("bucket");
        final String regionName = properties.getProperty("region");
        // get remote connection
        final S3ConnectionFactory factory = new S3ConnectionFactory(regionName);
        final ConnectionCache cache = userState.getS3().getCache();
        final S3ConnectionResource resource = (S3ConnectionResource) cache.getResource("", factory);
        final S3Client s3Client = resource.getConnection().getS3Client();
        // get remote resource
        final GetObjectRequest getObjectRequest = GetObjectRequest.builder()
                .bucket(bucketName).key(prefix).build();
        final ResponseBytes<GetObjectResponse> objectAsBytes = s3Client.getObjectAsBytes(getObjectRequest);
        // serve resource
        final byte[] payload = objectAsBytes.asByteArray();
        final long lastModified = objectAsBytes.response().lastModified().toEpochMilli();
        final FileMetaData metaData = new FileMetaData(null, payload.length, lastModified, false);
        final Preferences preferences = new Preferences(userState.getConfig());
        final String mimeType = Value.defaultOnEmpty(preferences.getMIMEType(prefix), Http.Mime.TEXT_PLAIN_UTF8);
        return HttpResponseU.to200(new MetaFile(metaData, mimeType, new ByteArrayInputStream(payload)));
    }

    private HttpResponse doGetFolder(final String prefix) throws IOException {
        final String bucketName = properties.getProperty("bucket");
        final String regionName = properties.getProperty("region");
        final S3ConnectionFactory factory = new S3ConnectionFactory(regionName);
        final ConnectionCache cache = userState.getS3().getCache();
        final S3ConnectionResource resource = (S3ConnectionResource) cache.getResource("", factory);
        final S3Client s3Client = resource.getConnection().getS3Client();
        final ResourceCache cache1 = userState.getCache();
        final RowSetSource rowSetSource = new CacheRowSetSource(cache1, new S3RowSetSource(
                s3Client, httpRequest.getBaseURI(), bucketName, prefix), prefix);
        final RowSet rowSet;
        try {
            rowSet = rowSetSource.getRowSet();
        } catch (Exception e) {
            throw new IOException(e);
        }
        cache1.clear();  // debugging aid
        final UserStateTable table = new UserStateTable(userState, null, httpRequest.getDate(), true);
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element body = new XPather(html, null).getElement(Html.XPath.CONTENT);
        table.toTableView(rowSet).addContentTo(body);
        final String labelContext = Value.wrap("[", "]", "label");
        final AppTitle appTitle = AppTitle.Factory.getResourceLabel(httpRequest, userState.getBundle(), labelContext);
        final MenuSystem menuSystem = userState.getMenuSystem();
        final MenuContext menuContext = new MenuContext(menuSystem, Collections.emptyList());
        return new AppHtmlView(httpRequest, userState, appTitle, menuContext, "").fixup(html);
    }
}
