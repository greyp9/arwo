package io.github.greyp9.arwo.app.mail.pop3.core;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.value.Value;

public class POP3Request {
    private final AppRequest request;
    private final AppUserState userState;
    private final Pather patherServer;
    private final Pather patherFolder;
    private final Pather patherMessage;

    public final ServletHttpRequest getHttpRequest() {
        return request.getHttpRequest();
    }

    public final AppUserState getUserState() {
        return userState;
    }

    public final Locus getLocus() {
        return request.getLocus();
    }

    public final Bundle getBundle() {
        return request.getBundle();
    }

    public final Alerts getAlerts() {
        return request.getAlerts();
    }

    public POP3Request(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = userState.getAppRequest(httpRequest);
        this.userState = userState;
        this.patherServer = new Pather(httpRequest.getPathInfo());
        this.patherFolder = new Pather(patherServer.getRight());
        this.patherMessage = new Pather(patherFolder.getRight());
    }

    public final String getServer() {
        return patherServer.getLeftToken();
    }

    public final String getFolder() {
        final String folder = patherFolder.getLeftToken();
        return Value.isEmpty(folder) ? null : folder;
    }

    public final String getMessage() {
        final String message = patherMessage.getLeftToken();
        return Value.isEmpty(message) ? null : message;
    }

    public final String getTitlePath() {
        return "[POP3] " + Value.join(Http.Token.COLON, getServer());
    }
}
