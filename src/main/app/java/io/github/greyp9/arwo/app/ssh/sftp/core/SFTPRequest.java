package io.github.greyp9.arwo.app.ssh.sftp.core;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.value.Value;

import java.util.Locale;

public class SFTPRequest {
    private final AppRequest appRequest;
    private final AppUserState userState;
    private final Pather patherMode;
    private final Pather patherServer;

    public final AppRequest getAppRequest() {
        return appRequest;
    }

    public final ServletHttpRequest getHttpRequest() {
        return appRequest.getHttpRequest();
    }

    public final AppUserState getUserState() {
        return userState;
    }

    public final Locale getLocale() {
        return appRequest.getLocus().getLocale();
    }

    public final Bundle getBundle() {
        return appRequest.getBundle();
    }

    public final Alerts getAlerts() {
        return appRequest.getAlerts();
    }

    public SFTPRequest(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.appRequest = ((userState == null) ?
                new AppRequest(httpRequest, null, null, null, null) : userState.getAppRequest(httpRequest));
        this.userState = userState;
        this.patherMode = new Pather(httpRequest.getPathInfo());
        this.patherServer = new Pather(patherMode.getRight());
    }

    public final String getBaseURIMode() {
        return appRequest.getHttpRequest().getBaseURI() + patherMode.getLeft();
    }

    public final String getBaseURIServer() {
        return getBaseURIMode() + patherServer.getLeft();
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

    public final String getTitlePath() {
        return Value.join(Http.Token.COLON, getServer(), getPath());
    }
}
