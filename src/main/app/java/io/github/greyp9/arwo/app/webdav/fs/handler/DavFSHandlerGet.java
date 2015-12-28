package io.github.greyp9.arwo.app.webdav.fs.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.webdav.fs.core.DavFSRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;

public class DavFSHandlerGet {
    private final DavFSRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public DavFSHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = new DavFSRequest(httpRequest, userState);
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
            httpResponse = HttpResponseU.to302(PathU.toDir(baseURI, App.Mode.VIEW));
        } else {
            //httpResponse = new DavFSResourceView(request, userState).doGetResource(request.getPath());
            httpResponse = HttpResponseU.to501();
        }
        return httpResponse;
    }
}
