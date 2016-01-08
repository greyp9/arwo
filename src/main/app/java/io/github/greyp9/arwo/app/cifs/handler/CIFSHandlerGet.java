package io.github.greyp9.arwo.app.cifs.handler;

import io.github.greyp9.arwo.app.cifs.connection.CIFSConnectionFactory;
import io.github.greyp9.arwo.app.cifs.connection.CIFSConnectionResource;
import io.github.greyp9.arwo.app.cifs.core.CIFSRequest;
import io.github.greyp9.arwo.app.cifs.view.CIFSInventoryXView;
import io.github.greyp9.arwo.app.cifs.view.CIFSResourceView;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;

public class CIFSHandlerGet {
    private final CIFSRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public CIFSHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = new CIFSRequest(httpRequest, userState);
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

    private HttpResponse doGet2() throws IOException {
        HttpResponse httpResponse;
        final String baseURI = httpRequest.getBaseURI();
        final String pathInfo = httpRequest.getPathInfo();
        if (pathInfo == null) {
            httpResponse = HttpResponseU.to302(PathU.toDir(baseURI));
        } else if (Value.isEmpty(request.getMode())) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getBaseURI(), App.Mode.VIEW));
        } else if (Value.isEmpty(request.getServer())) {
            httpResponse = new CIFSInventoryXView(request, userState, null, App.Mode.VIEW).doGetResponse();
        } else {
            httpResponse = doGet3();
        }
        return httpResponse;
    }

    private HttpResponse doGet3() throws IOException {
        HttpResponse httpResponse;
        final CIFSConnectionFactory factory = new CIFSConnectionFactory(
                httpRequest, userState, request.getBundle(), request.getAlerts());
        final CIFSConnectionResource resource = (CIFSConnectionResource)
                userState.getCIFS().getCache().getResource(request.getServer(), factory);
        if (resource == null) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getBaseURI(), App.Mode.VIEW));
        } else {
            httpResponse = new CIFSResourceView(request, userState, resource).doGetResource(request.getPath());
        }
        return httpResponse;
    }
}
