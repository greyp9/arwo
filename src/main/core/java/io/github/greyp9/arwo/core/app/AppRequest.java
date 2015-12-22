package io.github.greyp9.arwo.core.app;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;

public class AppRequest {
    private final ServletHttpRequest httpRequest;
    private final String submitID;
    private final Locus locus;
    private final Bundle bundle;
    private final Alerts alerts;

    public final ServletHttpRequest getHttpRequest() {
        return httpRequest;
    }

    public final String getSubmitID() {
        return submitID;
    }

    public final Locus getLocus() {
        return locus;
    }

    public final Bundle getBundle() {
        return bundle;
    }

    public final Alerts getAlerts() {
        return alerts;
    }

    public AppRequest(final ServletHttpRequest httpRequest, final String submitID,
                      final Locus locus, final Bundle bundle, final Alerts alerts) {
        this.httpRequest = httpRequest;
        this.submitID = submitID;
        this.locus = locus;
        this.bundle = bundle;
        this.alerts = alerts;
    }
}
