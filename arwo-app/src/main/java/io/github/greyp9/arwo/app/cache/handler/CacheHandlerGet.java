package io.github.greyp9.arwo.app.cache.handler;

import io.github.greyp9.arwo.app.cache.core.Cache;
import io.github.greyp9.arwo.app.cache.view.CacheInventoryView;
import io.github.greyp9.arwo.app.cache.view.MetaFileInventoryView;
import io.github.greyp9.arwo.app.cache.view.MetaFileView;
import io.github.greyp9.arwo.app.cache.view.RowSetView;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.Pather;

import java.io.IOException;

public class CacheHandlerGet {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final ResourceCache cache;

    public CacheHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        //final String servletPath = httpRequest.getServletPath();  // possible means to resolve relevant cache
        this.cache = userState.getCache();
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
        HttpResponse httpResponse;
        final Pather pather = new Pather(httpRequest.getPathInfo());
        final String view = pather.getLeftToken();
        if (Cache.CONTEXT_METAFILES.equals(view)) {
            httpResponse = doGetMetaFile(pather.getRight());
        } else if (Cache.CONTEXT_ROWSETS.equals(view)) {
            httpResponse = doGetRowSet(pather.getRight());
        } else {
            httpResponse = new CacheInventoryView(httpRequest, userState, userState.getCache()).render();
        }
        return httpResponse;
    }

    private HttpResponse doGetMetaFile(final String pathInfo) throws IOException {
        final HttpResponse httpResponse;
        if (Http.Token.SLASH.equals(pathInfo)) {
            httpResponse = new MetaFileInventoryView(httpRequest, userState, cache).render();
        } else {
            httpResponse = new MetaFileView(httpRequest, userState, cache).render(pathInfo);
        }
        return httpResponse;
    }

    private HttpResponse doGetRowSet(final String pathInfo) throws IOException {
        final HttpResponse httpResponse;
        if (!cache.containsRowSet(pathInfo)) {
            httpResponse = HttpResponseU.to404();
        } else {
            httpResponse = new RowSetView(httpRequest, userState, cache.getRowSet(pathInfo)).render();
        }
        return httpResponse;
    }
}
