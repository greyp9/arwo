package io.github.greyp9.arwo.app.cache.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;

import java.io.IOException;

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
            final byte[] bytes = StreamU.read(metaFile.getBytes());
            httpResponse = HttpResponseU.to200(bytes);
        }
        return httpResponse;
    }
}
