package io.github.greyp9.arwo.app.ssh.sftp.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionFactory;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.core.view.SSHView;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.view.SFTPResourceView;
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

    public final HttpResponse doGet() throws IOException {
        HttpResponse httpResponse;
        final String baseURI = httpRequest.getBaseURI();
        final String pathInfo = httpRequest.getPathInfo();
        if (pathInfo == null) {
            httpResponse = HttpResponseU.to302(PathU.toDir(baseURI));
        } else if (Value.isEmpty(request.getMode())) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getBaseURI(), Const.MODE_DEFAULT));
        } else if (Value.isEmpty(request.getServer())) {
            httpResponse = new SSHView(request.getHttpRequest(), userState).doGetHtmlInventory(Const.MODE_DEFAULT);
        } else {
            httpResponse = doGet2();
        }
        return httpResponse;
    }

    private HttpResponse doGet2() throws IOException {
        HttpResponse httpResponse;
        final SSHConnectionFactory factory = new SSHConnectionFactory(httpRequest, userState);
        final SSHConnectionResource resource = (SSHConnectionResource)
                userState.getCacheSSH().getResource(request.getServer(), factory);
        if (resource == null) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getBaseURI(), Const.MODE_DEFAULT));
        } else {
            httpResponse = new SFTPResourceView(request, userState, resource).doGetResource(request.getPath());
        }
        return httpResponse;
    }

    private static class Const {
        private static final String MODE_DEFAULT = "view";
    }
}
