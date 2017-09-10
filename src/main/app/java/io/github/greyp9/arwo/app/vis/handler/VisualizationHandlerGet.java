package io.github.greyp9.arwo.app.vis.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.vis.core.VisualizationRequest;
import io.github.greyp9.arwo.app.vis.view.VisualizationEntryView;
import io.github.greyp9.arwo.app.vis.view.VisualizationInventoryView;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;

public class VisualizationHandlerGet {
    private final VisualizationRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public VisualizationHandlerGet(ServletHttpRequest httpRequest, AppUserState userState) {
        this.request = new VisualizationRequest(httpRequest, userState);
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doGetSafe() throws IOException {
        HttpResponse httpResponse;
        try {
            httpResponse = doGet();
        } catch (IOException e) {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
            httpResponse = HttpResponseU.to500(e.getMessage());
        }
        return httpResponse;
    }

    private HttpResponse doGet() throws IOException {
        HttpResponse httpResponse;
        final String baseURI = httpRequest.getBaseURI();
        final String pathInfo = httpRequest.getPathInfo();
        final boolean isPathInfo = (pathInfo != null);
        final boolean isTrailingSlash = (isPathInfo && (pathInfo.endsWith(Http.Token.SLASH)));
        final boolean isNoPathInfo = (!isPathInfo);
        final boolean isNoTrailingSlash = (!isTrailingSlash);
        if (isNoPathInfo) {
            httpResponse = HttpResponseU.to302(PathU.toDir(baseURI));
        } else if (isNoTrailingSlash) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getURI()));
        } else if (Value.isEmpty(request.getContext())) {
            httpResponse = new VisualizationInventoryView(httpRequest, request, userState).doGetResponse();
        } else {
            httpResponse = doGet2();
        }
        return httpResponse;
    }

    private HttpResponse doGet2() throws IOException {
        HttpResponse httpResponse;
        try {
            httpResponse = new VisualizationEntryView(httpRequest, request, userState).doGetResponse();
        } catch (IOException e) {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
            httpResponse = HttpResponseU.to302(httpRequest.getBaseURI());
        }
        return httpResponse;
    }
}
