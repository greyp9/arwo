package io.github.greyp9.arwo.app.cache.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;
import java.net.HttpURLConnection;

public class CacheHandlerGet {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public CacheHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doGet() throws IOException {
        HttpResponse httpResponse;
        final MetaFile metaFile = userState.getCacheBlob().getFile(httpRequest.getPathInfo());
        if (metaFile == null) {
            httpResponse = HttpResponseU.to404();
        } else {
            final NameTypeValues headers = new NameTypeValues();
            final String mimeTypeOverride = userState.getProperties().getProperty(App.Action.MIME_TYPE);
            final String mimeType = Value.defaultOnEmpty(mimeTypeOverride, Http.Mime.TEXT_PLAIN_UTF8);
            headers.add(new NameTypeValue(Http.Header.CONTENT_TYPE, mimeType));
            httpResponse = new HttpResponse(HttpURLConnection.HTTP_OK, headers, metaFile.getBytes());
        }
        return httpResponse;
    }
}
