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
    private final Pather patherMetric;
    private final Pather patherMode;
    private final Pather patherPage;
    private final Pather patherScale;

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
        this.patherContext = new Pather(httpRequest.getPathInfo());  // "/irby:servlet/irby:init-param/@name"
        this.patherMetric = new Pather(patherContext.getRight());  //
        this.patherMode = new Pather(patherMetric.getRight());  // html, text
        this.patherPage = new Pather(patherMode.getRight());  // 2000-01-01T00-00Z
        this.patherScale = new Pather(patherPage.getRight());  // 2, 3
    }

    public final String getContext() {
        return patherContext.getLeftToken();
    }

    public final String getMetric() {
        final String leftToken = patherMetric.getLeftToken();
        return ("-".equals(leftToken) ? getContext() : leftToken);
    }

    public final String getMode() {
        return patherMode.getLeftToken();
    }

    public final String getPage() {
        return patherPage.getLeftToken();
    }

    public final String getScale() {
        return patherScale.getLeftToken();
    }

    public final String getPath() {
        final String path = patherScale.getRight();
        return Value.defaultOnNull(path, "");
    }
}
