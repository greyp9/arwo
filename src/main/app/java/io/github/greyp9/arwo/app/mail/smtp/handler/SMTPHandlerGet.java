package io.github.greyp9.arwo.app.mail.smtp.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.mail.smtp.core.SMTPRequest;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;

public class SMTPHandlerGet {
    private final SMTPRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public SMTPHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = new SMTPRequest(httpRequest, userState);
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doGet() throws IOException {
        HttpResponse httpResponse;
        final String baseURI = httpRequest.getBaseURI();
        final String pathInfo = httpRequest.getPathInfo();
        final String query = httpRequest.getHttpRequest().getQuery();
        final boolean isPathInfo = (pathInfo != null);
        final boolean isTrailingSlash = (isPathInfo && (pathInfo.endsWith(Http.Token.SLASH)));
        final boolean isQuery = (query != null);
        final boolean isNoPathInfo = (!isPathInfo);
        final boolean isNoTrailingSlash = (!isTrailingSlash);
        if (isNoPathInfo) {
            httpResponse = HttpResponseU.to302(PathU.toDir(baseURI));
        } else if (isNoTrailingSlash) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getURI()));
        } else if (isQuery) {
            httpResponse = HttpResponseU.to302(httpRequest.getURI());
        } else if (Value.isEmpty(request.getServer())) {
            userState.getClass();
            //httpResponse = new SMTPInventoryView(request, userState, "").doGetResponse();
            httpResponse = HttpResponseU.to501();
        } else {
            //httpResponse = new SMTPCommandView(request, userState).doGetResponse();
            httpResponse = HttpResponseU.to501();
        }
        return httpResponse;
    }
}