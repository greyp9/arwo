package io.github.greyp9.arwo.app.dash.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.dash.view.DashView;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;

import java.io.IOException;

public class DashHandlerGet {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public DashHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doGet() throws IOException {
        HttpResponse httpResponse;
        final String baseURI = httpRequest.getBaseURI();
        final String pathInfo = httpRequest.getPathInfo();
        if (pathInfo == null) {
            httpResponse = HttpResponseU.to302(PathU.toDir(baseURI));
        } else {
            httpResponse = new DashView(httpRequest, userState).doGetResponse();
        }
        return httpResponse;
    }
}