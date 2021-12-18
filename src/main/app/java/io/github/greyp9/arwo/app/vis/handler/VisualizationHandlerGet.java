package io.github.greyp9.arwo.app.vis.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.vis.core.VisualizationRequest;
import io.github.greyp9.arwo.app.vis.view.VisualizationEntryView;
import io.github.greyp9.arwo.app.vis.view.VisualizationHistoryView;
import io.github.greyp9.arwo.app.vis.view.VisualizationInventoryView;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogram;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;
import java.util.Map;
import java.util.Properties;

public class VisualizationHandlerGet {
    private final VisualizationRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public VisualizationHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
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
        } else if (Value.isEmpty(request.getMode())) {
            httpResponse = HttpResponseU.to302(PathU.toDir(baseURI, request.getContext(), "-", Html.HTML));
        } else {
            httpResponse = doGet2();
        }
        return httpResponse;
    }

    private HttpResponse doGet2() throws IOException {
        HttpResponse httpResponse;
        final NameTypeValue config = getConfigForContext(httpRequest.getInitParams());
        if (config == null) {
            httpResponse = HttpResponseU.to404();
        } else if (TimeHistogram.class.getName().equals(config.getValueS())) {
            httpResponse = doGet3(config.getName());
        } else {
            httpResponse = HttpResponseU.to404();
        }
        return httpResponse;
    }

    private HttpResponse doGet3(final String name) throws IOException {
        HttpResponse httpResponse;
        final TimeHistogram histogram = (TimeHistogram) AppNaming.lookup(App.Application.LOOKUP, name);
        if (histogram == null) {
            httpResponse = HttpResponseU.to404();
        } else if (Html.FILE.equals(request.getMode())) {
            httpResponse = new VisualizationHistoryView(httpRequest, request, userState, histogram).doGetResponse();
        } else {
            httpResponse = new VisualizationEntryView(httpRequest, request, userState, histogram).doGetResponse();
        }
        return httpResponse;
    }

    private NameTypeValue getConfigForContext(final Properties initParams) {
        NameTypeValue ntv = null;
        final String context = request.getContext();
        for (Map.Entry<Object, Object> entryIt : initParams.entrySet()) {
            final String key = (String) entryIt.getKey();
            if (key.equals(context)) {
                ntv = new NameTypeValue(key, entryIt.getValue());
                break;
            }
        }
        return ntv;
    }
}
