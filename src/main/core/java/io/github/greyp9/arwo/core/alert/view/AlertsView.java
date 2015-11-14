package io.github.greyp9.arwo.core.alert.view;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;

public class AlertsView {
    private final Alerts alerts;
    private final Locus locus;

    public AlertsView(final Alerts alerts, final Locus locus) {
        this.alerts = alerts;
        this.locus = locus;
    }

    public final void addContentTo(final Element html) throws IOException {
        final Collection<Alert> alertsDisplay = new ArrayList<Alert>(alerts.removeAll());
        if (!alertsDisplay.isEmpty()) {
            final NameTypeValues styleNotifies = NTV.create(Html.CLASS, "notifications");
            final Element divNotifications = ElementU.addElementFirst(html, Html.DIV, null, styleNotifies);
            for (final Alert alert : alertsDisplay) {
                final String severity = alert.getSeverity().toString().toLowerCase(Locale.ENGLISH);
                final String severityClass = Value.join(Html.SPACE, "level", severity);
                final NameTypeValues styleNotify = NTV.create(Html.CLASS, "notification");
                final NameTypeValues styleSeverity = NTV.create(Html.CLASS, severityClass);
                final NameTypeValues styleTimestamp = NTV.create(Html.CLASS, "timestamp");
                final NameTypeValues styleText = NTV.create(Html.CLASS, "text");
                final Element divNotify = ElementU.addElement(divNotifications, Html.DIV, null, styleNotify);
                ElementU.addElement(divNotify, Html.SPAN, alert.getIcon(), styleSeverity);
                ElementU.addElement(divNotify, Html.SPAN, locus.toString(alert.getDate()), styleTimestamp);
                ElementU.addElement(divNotify, Html.SPAN, alert.getMessage(), styleText);
                if (alert.getDetail() != null) {
                    ElementU.addElement(divNotify, Html.PRE, alert.getDetail(), styleText);
                }
                if (Alert.Severity.ERR == alert.getSeverity()) {
                    logAlert(alert);
                }
            }
        }
    }

    private void logAlert(final Alert alert) throws IOException {
        final Logger logger = Logger.getLogger(getClass().getName());
        logger.log(toLogLevel(alert.getSeverity()), alert.toString());
    }

    private Level toLogLevel(final Alert.Severity severity) throws IOException {
        Level level = Level.SEVERE;
        if (severity == Alert.Severity.INFO) {
            level = Level.INFO;
        } else if (severity == Alert.Severity.WARN) {
            level = Level.WARNING;
        }
        return level;
    }
}
