package io.github.greyp9.arwo.app.ssh.sftp.core;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.AppText;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.value.Value;

import java.util.Locale;

public class SFTPRequest {
    private final ServletHttpRequest httpRequest;
    private final Pather patherMode;
    private final Pather patherServer;
    private final Locale locale;
    private final Bundle bundle;
    private final Alerts alerts;

    public final ServletHttpRequest getHttpRequest() {
        return httpRequest;
    }

    public final Locale getLocale() {
        return locale;
    }

    public final Bundle getBundle() {
        return bundle;
    }

    public final Alerts getAlerts() {
        return alerts;
    }

    public SFTPRequest(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.patherMode = new Pather(httpRequest.getPathInfo());
        this.patherServer = new Pather(patherMode.getRight());
        this.locale = userState.getLocus().getLocale();
        this.bundle = new Bundle(new AppText(locale).getBundleCore());
        this.alerts = userState.getAlerts();
    }

    public final String getBaseURIMode() {
        return httpRequest.getBaseURI() + patherMode.getLeft();
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
        return "[SFTP] " + Value.join(Http.Token.COLON, getServer(), getPath());
    }
}
