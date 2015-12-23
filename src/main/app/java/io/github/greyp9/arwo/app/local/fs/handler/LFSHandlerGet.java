package io.github.greyp9.arwo.app.local.fs.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.app.local.fs.view.LFSResourceView;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;

public class LFSHandlerGet {
    private final LFSRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public LFSHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = new LFSRequest(httpRequest, userState);
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doGet() throws IOException {
        HttpResponse httpResponse;
        try {
            httpResponse = doGet2();
        } catch (IOException e) {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
            httpResponse = HttpResponseU.to302(httpRequest.getBaseURI());
        }
        return httpResponse;
    }

    public final HttpResponse doGet2() throws IOException {
        HttpResponse httpResponse;
        final String baseURI = httpRequest.getBaseURI();
        final String pathInfo = httpRequest.getPathInfo();
        if (pathInfo == null) {
            httpResponse = HttpResponseU.to302(PathU.toDir(baseURI));
        } else if (Value.isEmpty(request.getMode())) {
            httpResponse = HttpResponseU.to302(PathU.toDir(baseURI, Const.MODE_DEFAULT));
        } else {
            httpResponse = new LFSResourceView(request, userState).doGetResource(request.getPath());
        }
        return httpResponse;
    }

    private static class Const {
        private static final String MODE_DEFAULT = "view";
    }
}
