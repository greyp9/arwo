package io.github.greyp9.arwo.app.jdbc.sh.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.jdbc.sh.core.JDBCRequest;
import io.github.greyp9.arwo.app.jdbc.sh.view.JDBCCommandView;
import io.github.greyp9.arwo.app.jdbc.sh.view.JDBCInventoryXView;
import io.github.greyp9.arwo.app.jdbc.sh.view.JDBCSearchView;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;

public class JDBCHandlerGet {
    private final JDBCRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public JDBCHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = new JDBCRequest(httpRequest, userState);
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
        } else if ("search".equals(request.getServer())) {
            httpResponse = new JDBCSearchView(request, userState).doGetResponse();
        } else if (Value.isEmpty(request.getServer())) {
            httpResponse = new JDBCInventoryXView(request, userState).doGetResponse();
        } else {
            httpResponse = new JDBCCommandView(request, userState).doGetResponse();
        }
        return httpResponse;
    }
}
