package io.github.greyp9.arwo.app.ssh.sftp.core;

import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.Pather;

public class SFTPRequest {
    private final ServletHttpRequest httpRequest;
    private final Pather patherMode;
    private final Pather patherServer;

    public final ServletHttpRequest getHttpRequest() {
        return httpRequest;
    }

    public SFTPRequest(final ServletHttpRequest httpRequest) {
        this.httpRequest = httpRequest;
        this.patherMode = new Pather(httpRequest.getPathInfo());
        this.patherServer = new Pather(patherMode.getRight());
    }

    public final String getMode() {
        return patherMode.getLeftToken();
    }

    public final String getServer() {
        return patherServer.getLeftToken();
    }

    public final String getPath() {
        return patherServer.getRight();
    }
}
