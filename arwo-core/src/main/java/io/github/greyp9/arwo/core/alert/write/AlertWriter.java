package io.github.greyp9.arwo.core.alert.write;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.link.AlertLink;
import io.github.greyp9.arwo.core.alert.link.AlertLinks;
import io.github.greyp9.arwo.core.bundle.Bundle;

public class AlertWriter {
    private final Bundle bundle;
    private final Alerts alerts;
    private final Alert.Severity severity;

    public AlertWriter(final Bundle bundle, final Alerts alerts) {
        this(bundle, alerts, null);
    }

    public AlertWriter(final Bundle bundle, final Alerts alerts, final Alert.Severity severity) {
        this.bundle = bundle;
        this.alerts = alerts;
        this.severity = severity;
    }

    public final void write(final String keyAlert, final String keyHref, final String href) {
        if ((bundle != null) && (alerts != null) && (keyAlert != null) && (keyHref != null) && (href != null)) {
            final AlertLink link = new AlertLink(null, bundle.getString(keyHref), null, href, null);
            alerts.add(new Alert(severity, bundle.getString(keyAlert), new AlertLinks(link)));
        }
    }
}
