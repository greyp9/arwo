package io.github.greyp9.arwo.app.cache.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.file.tar.TarMetaData;
import io.github.greyp9.arwo.core.file.tar.TarVolume;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Collection;

public final class MetaFileView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final ResourceCache cache;

    public MetaFileView(final ServletHttpRequest httpRequest,
                        final AppUserState userState,
                        final ResourceCache cache) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.cache = cache;
    }

    public HttpResponse render(final String pathInfo) throws IOException {
        HttpResponse httpResponse;
        final Pather pather = new Pather(pathInfo);
        final String left = pather.getLeft();
        final String right = pather.getRight();
        final String leftMinusBang = left.endsWith("!") ? left.substring(0, left.length() - 1) : left;
        final MetaFile metaFile = cache.getFile(leftMinusBang);
        final FileMetaData metaData = (metaFile == null) ? null : metaFile.getMetaData();
        final String path = (metaData == null) ? null : metaData.getPath();
        if ((metaFile == null) || (metaData == null)) {
            httpResponse = HttpResponseU.to404();
        } else if (left.contains("!")) {
            httpResponse = renderEntry(path, metaFile, right);
        } else if (path.endsWith(".tar.gz")) {
            httpResponse = renderTGZ(pathInfo, metaFile);
        } else {
            httpResponse = render(pathInfo, metaFile);
        }
        return httpResponse;
    }

    private HttpResponse render(final String path, final MetaFile metaFile) throws IOException {
        final String contentType = Value.defaultOnEmpty(
                metaFile.getContentType(),
                userState.getProperties().getProperty(App.Action.MIME_TYPE),
                new Preferences(userState.getConfig()).getMIMEType(path),
                Http.Mime.TEXT_PLAIN_UTF8);
        final NameTypeValues headers = new NameTypeValues(
                new NameTypeValue(Http.Header.CONTENT_TYPE, contentType));
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, metaFile.getBytes());
    }

    private HttpResponse renderEntry(
            final String path,
            final MetaFile metaFile,
            final String right) throws IOException {
        final Pather pather = new Pather(right);
        final String filename = pather.getLeftToken();
        final TarVolume tarVolume = new TarVolume(metaFile.getBytes());
        final Collection<TarMetaData> tarVolumeEntries = tarVolume.getEntries();
        for (TarMetaData tarMetaData : tarVolumeEntries) {
            if (Value.equal(filename, tarMetaData.getPath())) {
                final MetaFile metaFileEntry = tarVolume.getEntry(filename);
                return render(metaFileEntry.getMetaData().getPath(), metaFileEntry);
            }
        }
        return HttpResponseU.toError(HttpURLConnection.HTTP_NOT_IMPLEMENTED, path);
    }

    private HttpResponse renderTGZ(final String pathInfo, final MetaFile metaFile) throws IOException {
        if (!cache.containsRowSet(pathInfo)) {
            final TarVolume tarVolume = new TarVolume(metaFile.getBytes());
            final MetaFileRowSetTGZ metaFileRowSetTGZ = new MetaFileRowSetTGZ(
                    pathInfo, httpRequest.getBaseURI(), tarVolume);
            cache.putRowSet(pathInfo, metaFileRowSetTGZ.getRowSet());
        }
        return HttpResponseU.to302("/arwo/cache/r" + pathInfo);
    }
}
