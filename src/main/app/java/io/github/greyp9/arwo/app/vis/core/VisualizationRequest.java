package io.github.greyp9.arwo.app.vis.core;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.value.Value;

public class VisualizationRequest {
    private final AppRequest appRequest;
    private final AppUserState userState;
    private final Pather patherContext;
    private final Pather patherMode;

    public final AppRequest getAppRequest() {
        return appRequest;
    }

    public final ServletHttpRequest getHttpRequest() {
        return appRequest.getHttpRequest();
    }

    public final AppUserState getUserState() {
        return userState;
    }

    public VisualizationRequest(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.appRequest = userState.getAppRequest(httpRequest);
        this.userState = userState;
        this.patherContext = new Pather(httpRequest.getPathInfo());
        this.patherMode = new Pather(patherContext.getRight());
    }

    public final String getContext() {
        return patherContext.getLeftToken();
    }

    public final String getMode() {
        return patherMode.getLeftToken();
    }

    public final String getPath() {
        final String path = patherMode.getRight();
        return Value.defaultOnNull(path, "");
    }
}
