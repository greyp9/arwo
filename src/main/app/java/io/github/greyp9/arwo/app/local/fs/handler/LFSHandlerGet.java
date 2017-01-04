package io.github.greyp9.arwo.app.local.fs.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.app.local.fs.resource.LFSResource;
import io.github.greyp9.arwo.app.local.fs.view.LFSResourceView;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.File;
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
            httpResponse = HttpResponseU.to302(PathU.toDir(baseURI, App.Mode.VIEW));
        } else if (Value.isEmpty(request.getFolder())) {
            httpResponse = HttpResponseU.to404();
        } else {
            httpResponse = doGet3();
        }
        return httpResponse;
    }

    private HttpResponse doGet3() throws IOException {
        HttpResponse httpResponse;
        final File folderBase = LFSResource.getFolderBase(request);
        if (folderBase == null) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getBaseURI(), App.Mode.VIEW));
        } else {
            httpResponse = new LFSResourceView(request, userState, folderBase).doGetResource(request.getPath());
        }
        return httpResponse;
    }
}
