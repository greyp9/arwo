package io.github.greyp9.arwo.app.local.sh.core;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.Pather;

import java.util.Locale;

public class SHRequest {
    private final AppRequest request;
    private final AppUserState userState;
    private final Pather patherContext;
    private final Pather patherScriptID;

    public final AppRequest getAppRequest() {
        return request;
    }

    public final ServletHttpRequest getHttpRequest() {
        return request.getHttpRequest();
    }

    public final AppUserState getUserState() {
        return userState;
    }

    public final Locale getLocale() {
        return request.getLocus().getLocale();
    }

    public final Bundle getBundle() {
        return request.getBundle();
    }

    public final Alerts getAlerts() {
        return request.getAlerts();
    }

    public SHRequest(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = userState.getAppRequest(httpRequest);
        this.userState = userState;
        this.patherContext = new Pather(httpRequest.getPathInfo());
        this.patherScriptID = new Pather(patherContext.getRight());
    }

    public final String getContext() {
        return patherContext.getLeftToken();
    }

    public final String getScriptID() {
        return patherScriptID.getLeftToken();
    }
}
