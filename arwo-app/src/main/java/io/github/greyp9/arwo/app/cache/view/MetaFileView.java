package io.github.greyp9.arwo.app.cache.view;

import io.github.greyp9.arwo.app.cache.core.Cache;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.file.tar.TarMetaData;
import io.github.greyp9.arwo.core.file.tar.TarVolume;
import io.github.greyp9.arwo.core.file.zip.ZipMetaData;
import io.github.greyp9.arwo.core.file.zip.ZipVolume;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Collection;

public final class MetaFileView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final ResourceCache cache;
    private final String resourceRowSets;

    public MetaFileView(final ServletHttpRequest httpRequest,
                        final AppUserState userState,
                        final ResourceCache cache) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.cache = cache;
        this.resourceRowSets = PathU.toPath(httpRequest.getBaseURI(), Cache.CONTEXT_ROWSETS);
    }

    public HttpResponse render(final String pathInfo) throws IOException {
        final HttpResponse httpResponse;
        final Pather pather = new Pather(pathInfo);
        final String left = pather.getLeft();
        final String right = pather.getRight();
        final String leftMinusBang = left.endsWith(App.Token.BANG) ? left.substring(0, left.length() - 1) : left;
        final MetaFile metaFile = cache.getFile(leftMinusBang);
        final FileMetaData metaData = (metaFile == null) ? null : metaFile.getMetaData();
        final String path = (metaData == null) ? null : metaData.getPath();
        final Preferences preferences = new Preferences(userState.getConfig());
        final String mimeType = (path == null) ? null : preferences.getMIMETypeIt(path);
        if ((metaFile == null) || (metaData == null)) {
            httpResponse = HttpResponseU.to404();
        } else if (left.contains(App.Token.BANG)) {
            httpResponse = renderEntry(path, metaFile, right);
        } else if (Http.Mime.APP_TGZ.equals(mimeType)) {
            httpResponse = renderTGZ(pathInfo, metaFile);
        } else if (Http.Mime.APP_ZIP.equals(mimeType)) {
            httpResponse = renderZip(pathInfo, metaFile);
        } else if (Http.Mime.APP_GZIP.equals(mimeType)) {
            httpResponse = render(pathInfo, metaFile, Http.Header.GZIP);
        } else {
            httpResponse = render(pathInfo, metaFile, null);
        }
        return httpResponse;
    }

    private HttpResponse render(final String path,
                                final MetaFile metaFile,
                                final String contentEncoding) throws IOException {
        final boolean mimeOverride =
                (Http.Header.GZIP.equals(contentEncoding) && Http.Mime.APP_GZIP.equals(metaFile.getContentType()));
        final String contentType = Value.defaultOnEmpty(
                mimeOverride ? Http.Mime.TEXT_PLAIN_UTF8 : metaFile.getContentType(),
                userState.getProperties().getProperty(App.Action.MIME_TYPE),
                new Preferences(userState.getConfig()).getMIMETypeIt(path),
                Http.Mime.TEXT_PLAIN_UTF8);
        final NameTypeValues headers = NameTypeValuesU.create(
                Http.Header.CONTENT_ENCODING, contentEncoding,
                Http.Header.CONTENT_TYPE, contentType,
                Http.Header.CONTENT_LENGTH, Long.toString(metaFile.getMetaData().getLength()));
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, metaFile.getBytes());
    }

    private HttpResponse renderEntry(final String path,
                                     final MetaFile metaFile,
                                     final String right) throws IOException {
        final HttpResponse httpResponse;
        final String entry = right.substring(Http.Token.SLASH.length());
        if (Http.Mime.APP_TGZ.equals(metaFile.getContentType())) {
            httpResponse = renderEntryTGZ(path, metaFile, entry);
        } else if (Http.Mime.APP_ZIP.equals(metaFile.getContentType())) {
            httpResponse = renderEntryZip(path, metaFile, entry);
        } else {
            httpResponse = render(path, metaFile, right);
        }
        return httpResponse;
    }

    private HttpResponse renderEntryTGZ(final String path,
                                        final MetaFile metaFileTGZ,
                                        final String entryTGZ) throws IOException {
        final TarVolume volume = new TarVolume(metaFileTGZ.getBytes());
        final Collection<TarMetaData> volumeEntries = volume.getEntries();
        for (TarMetaData metaData : volumeEntries) {
            if (Value.equal(entryTGZ, metaData.getPath())) {
                final MetaFile metaFileEntry = volume.getEntry(entryTGZ);
                return render(metaFileEntry.getMetaData().getPath(), metaFileEntry, null);
            }
        }
        return HttpResponseU.toError(HttpURLConnection.HTTP_NOT_FOUND, path);
    }

    private HttpResponse renderEntryZip(final String path,
                                        final MetaFile metaFileZip,
                                        final String entryZip) throws IOException {
        final ZipVolume volume = new ZipVolume(metaFileZip.getBytes());
        final Collection<ZipMetaData> volumeEntries = volume.getEntries();
        for (ZipMetaData metaData : volumeEntries) {
            if (Value.equal(entryZip, metaData.getPath())) {
                final MetaFile metaFileEntry = volume.getEntry(entryZip);
                return render(metaFileEntry.getMetaData().getPath(), metaFileEntry, null);
            }
        }
        return HttpResponseU.toError(HttpURLConnection.HTTP_NOT_FOUND, path);
    }

    private HttpResponse renderTGZ(final String pathInfo, final MetaFile metaFile) throws IOException {
        if (!cache.containsRowSet(pathInfo)) {
            final TarVolume volume = new TarVolume(metaFile.getBytes());
            final MetaFileRowSetTGZ metaFileRowSet = new MetaFileRowSetTGZ(
                    pathInfo, httpRequest.getBaseURI(), volume);
            cache.putRowSet(pathInfo, metaFileRowSet.getRowSet());
        }
        return HttpResponseU.to302(resourceRowSets + pathInfo);
    }

    private HttpResponse renderZip(final String pathInfo, final MetaFile metaFile) throws IOException {
        if (!cache.containsRowSet(pathInfo)) {
            final ZipVolume volume = new ZipVolume(metaFile.getBytes());
            final MetaFileRowSetZip metaFileRowSet = new MetaFileRowSetZip(
                    pathInfo, httpRequest.getBaseURI(), volume);
            cache.putRowSet(pathInfo, metaFileRowSet.getRowSet());
        }
        return HttpResponseU.to302(resourceRowSets + pathInfo);
    }
}
