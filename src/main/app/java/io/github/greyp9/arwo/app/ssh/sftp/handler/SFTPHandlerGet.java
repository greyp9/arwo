package io.github.greyp9.arwo.app.ssh.sftp.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionFactory;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.view.SFTPInventoryView;
import io.github.greyp9.arwo.app.ssh.sftp.view.SFTPResourceView;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;

public class SFTPHandlerGet {
    private final SFTPRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public SFTPHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = new SFTPRequest(httpRequest, userState);
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
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getBaseURI(), App.Mode.VIEW));
        } else if (Value.isEmpty(request.getServer())) {
            httpResponse = new SFTPInventoryView(request, userState, null, App.Mode.VIEW).doGetResponse();
        } else {
            httpResponse = doGet3();
        }
        return httpResponse;
    }

    private HttpResponse doGet3() throws IOException {
        HttpResponse httpResponse;
        final SSHConnectionFactory factory = new SSHConnectionFactory(
                httpRequest, userState, request.getBundle(), request.getAlerts());
        final SSHConnectionResource resource = (SSHConnectionResource)
                userState.getSSH().getCache().getResource(request.getServer(), factory);
        if (resource == null) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getBaseURI(), App.Mode.VIEW));
        } else {
            httpResponse = new SFTPResourceView(request, userState, resource).doGetResource(request.getPath());
        }
        return httpResponse;
    }
}
