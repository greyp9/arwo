package io.github.greyp9.arwo.app.jdbc.sh.core;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.value.Value;

import java.util.Locale;

public class JDBCRequest {
    private final AppRequest request;
    private final AppUserState userState;
    private final Pather patherServer;
    private final Pather patherQueryID;

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

    public JDBCRequest(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = userState.getAppRequest(httpRequest);
        this.userState = userState;
        this.patherServer = new Pather(httpRequest.getPathInfo());
        this.patherQueryID = new Pather(patherServer.getRight());
    }

    public final String getServer() {
        return patherServer.getLeftToken();
    }

    public final String getQueryID() {
        return patherQueryID.getLeftToken();
    }

    public final String getTitlePath() {
        return Value.join(Http.Token.COLON, getServer());
    }
}
