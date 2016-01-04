package io.github.greyp9.arwo.app.webdav.fs.data;

import com.github.sardine.DavResource;
import com.github.sardine.Sardine;
import com.github.sardine.impl.SardineException;
import io.github.greyp9.arwo.app.webdav.fs.core.WebDAVRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.model.ExceptionModel;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.lib.sardine.webdav.connection.WebDAVConnection;

import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.TreeMap;

public class WebDAVDataSource {
    private final WebDAVRequest request;
    private final WebDAVConnection connection;

    public WebDAVDataSource(final WebDAVRequest request, final WebDAVConnection connection) {
        this.request = request;
        this.connection = connection;
    }

    public final int lstat(final String path) throws IOException {
        int lstat;
        try {
            final List<DavResource> resources = listFiles(path);
            final DavResource firstResource = (resources.isEmpty() ? null : resources.get(0));
            final boolean isFolder = ((firstResource != null) && (firstResource.getPath().endsWith(Http.Token.SLASH)));
            final boolean isFile = ((firstResource != null) && (!isFolder));
            lstat = (isFolder ? App.FS.S_IFDIR : (isFile ? App.FS.S_IFREG : 0));
        } catch (SardineException e) {
            lstat = e.getStatusCode();
        }
        return lstat;
    }

    public final List<DavResource> listFiles(final String path) throws IOException {
        final Sardine sardine = connection.getConnection();
        final String url = connection.getURL().toExternalForm() + path;
        return sardine.list(url);
    }

    public final void delete(final String path) throws IOException {
        final Sardine sardine = connection.getConnection();
        final String url = connection.getURL().toExternalForm() + path;
        sardine.delete(url);
    }

    @SuppressWarnings("PMD.UseConcurrentHashMap")
    public final Map<String, List<DavResource>> find(final String path) throws IOException {
        final Map<String, List<DavResource>> find = new TreeMap<String, List<DavResource>>();
        final Stack<String> paths = new Stack<String>();
        paths.push(path);
        while (!paths.isEmpty()) {
            final String pathIt = paths.pop();
            final List<DavResource> resources = listFiles(pathIt);
            find.put(pathIt, resources);
        }
        return find;
    }

    public final MetaFile read(final String path) throws IOException {
        long lastModified = 0L;
        byte[] bytes = new byte[0];
        try {
            final Sardine sardine = connection.getConnection();
            final String url = connection.getURL().toExternalForm() + path;
            final List<DavResource> list = sardine.list(url);
            if (list.isEmpty()) {
                throw new FileNotFoundException(path);
            }
            final DavResource resource = list.iterator().next();
            final Date modified = resource.getModified();
            lastModified = ((modified == null) ? lastModified : modified.getTime());
            bytes = read(sardine.get(url));
            connection.update(request.getHttpRequest().getDate());
        } catch (IOException e) {
            new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
        }
        final FileMetaData metaData = new FileMetaData(path, bytes.length, lastModified, false);
        return new MetaFile(metaData, new ByteArrayInputStream(bytes));
    }

    private static byte[] read(final InputStream is) throws IOException {
        try {
            return StreamU.read(is);
        } finally {
            is.close();
        }
    }

    public final void write(final byte[] bytes, final String path) throws IOException {
        try {
            final Sardine sardine = connection.getConnection();
            sardine.put(path, bytes);
            connection.update(request.getHttpRequest().getDate());
        } catch (IOException e) {
            new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
        }
    }
}
