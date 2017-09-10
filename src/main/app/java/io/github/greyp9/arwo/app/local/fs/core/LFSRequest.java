package io.github.greyp9.arwo.app.local.fs.core;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.value.Value;

import java.io.UnsupportedEncodingException;
import java.util.Locale;

public class LFSRequest {
    private final AppRequest appRequest;
    private final AppUserState userState;
    private final Pather patherContext;
    private final Pather patherMode;
    private final Pather patherFolder;

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

    public LFSRequest(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.appRequest = userState.getAppRequest(httpRequest);
        this.userState = userState;
        this.patherContext = new Pather(httpRequest.getPathInfo());
        this.patherMode = new Pather(patherContext.getRight());
        this.patherFolder = new Pather(patherMode.getRight());
    }

    public final String getBaseURIMode() {
        return appRequest.getHttpRequest().getBaseURI() + patherContext.getLeft() + patherMode.getLeft();
    }

    public final String getBaseURIFolder() {
        return getBaseURIMode() + patherFolder.getLeft();
    }

    public final String getContext() {
        return patherContext.getLeftToken();
    }

    public final String getMode() {
        return patherMode.getLeftToken();
    }

    public final String getFolder() {
        return patherFolder.getLeftToken();
    }

    public final String getPath() {
        final String path = patherFolder.getRight();
        return Value.defaultOnNull(path, "");
    }

    public final String getTitlePath() throws UnsupportedEncodingException {
        return URLCodec.decode(Value.join("", patherFolder.getLeft(), getPath()));
    }
}
