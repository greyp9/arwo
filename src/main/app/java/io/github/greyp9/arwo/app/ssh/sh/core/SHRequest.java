package io.github.greyp9.arwo.app.ssh.sh.core;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.AppText;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.value.Value;

import java.util.Locale;

public class SHRequest {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Pather patherServer;
    private final Pather patherScriptID;
    private final Locale locale;
    private final Bundle bundle;
    private final Alerts alerts;

    public final ServletHttpRequest getHttpRequest() {
        return httpRequest;
    }

    public final AppUserState getUserState() {
        return userState;
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

    public SHRequest(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.patherServer = new Pather(httpRequest.getPathInfo());
        this.patherScriptID = new Pather(patherServer.getRight());
        this.locale = userState.getLocus().getLocale();
        this.bundle = new Bundle(new AppText(locale).getBundleCore());
        this.alerts = userState.getAlerts();
    }

    public final String getServer() {
        return patherServer.getLeftToken();
    }

    public final String getScriptID() {
        return patherScriptID.getLeftToken();
    }

    public final String getTitlePath() {
        return "[SSH] " + Value.join(Http.Token.COLON, getServer());
    }
}