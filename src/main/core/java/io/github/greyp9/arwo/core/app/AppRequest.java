package io.github.greyp9.arwo.core.app;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;

import java.util.Locale;

public class AppRequest {
    private final ServletHttpRequest httpRequest;
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

    public AppRequest(final ServletHttpRequest httpRequest, final Locale locale, final Bundle bundle,
                      final Alerts alerts) {
        this.httpRequest = httpRequest;
        this.locale = locale;
        this.bundle = bundle;
        this.alerts = alerts;
    }
}
