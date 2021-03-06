package io.github.greyp9.arwo.app.mail.imap.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.mail.imap.core.IMAPRequest;
import io.github.greyp9.arwo.app.mail.imap.view.IMAPFoldersView;
import io.github.greyp9.arwo.app.mail.imap.view.IMAPInventoryXView;
import io.github.greyp9.arwo.app.mail.imap.view.IMAPMessageView;
import io.github.greyp9.arwo.app.mail.imap.view.IMAPMessagesView;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.model.ExceptionModel;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;

public class IMAPHandlerGet {
    private final IMAPRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    //private final Bundle bundle;
    private final Alerts alerts;

    public IMAPHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = new IMAPRequest(httpRequest, userState);
        this.httpRequest = httpRequest;
        this.userState = userState;
        //this.bundle = request.getBundle();
        this.alerts = request.getAlerts();
    }

    public final HttpResponse doGetSafe() throws IOException {
        HttpResponse httpResponse;
        try {
            httpResponse = doGet();
        } catch (IOException e) {
            new ExceptionModel(alerts).service(new IOException(e), Alert.Severity.ERR);
            httpResponse = HttpResponseU.to302(httpRequest.getBaseURI());
        }
        return httpResponse;
    }

    private HttpResponse doGet() throws IOException {
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
            httpResponse = new IMAPInventoryXView(request, userState).doGetResponse();
        } else if (Value.isEmpty(request.getFolder())) {
            httpResponse = new IMAPFoldersView(request, userState).doGetResponse();
        } else if (Value.isEmpty(request.getMessage())) {
            httpResponse = new IMAPMessagesView(request, userState).doGetResponse();
        } else {
            httpResponse = new IMAPMessageView(request, userState).doGetResponse();
        }
        return httpResponse;
    }
}
