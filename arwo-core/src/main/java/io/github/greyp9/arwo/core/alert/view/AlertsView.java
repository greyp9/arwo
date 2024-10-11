package io.github.greyp9.arwo.core.alert.view;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.action.AlertActions;
import io.github.greyp9.arwo.core.alert.link.AlertLink;
import io.github.greyp9.arwo.core.alert.link.AlertLinks;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.HtmlU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.submit.SubmitTokenU;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;

public class AlertsView {
    private final boolean display;
    private final Alerts alerts;
    private final Locus locus;
    private final Bundle bundle;
    private final String submitID;

    public AlertsView(final boolean display, final Alerts alerts,
                      final Locus locus, final Bundle bundle, final String submitID) {
        this.display = display;
        this.alerts = alerts;
        this.locus = locus;
        this.bundle = bundle;
        this.submitID = submitID;
    }

    public final void addContentTo(final Element html) throws IOException {
        if (display) {
            addContentTo(html, alerts.getTransient());
            addContentTo(html, alerts.getPersistent());
        }
    }

    private void addContentTo(final Element html, final Collection<Alert> alertsDisplay) throws IOException {
        if (!alertsDisplay.isEmpty()) {
            final NameTypeValues styleNotifies = NTV.create(Html.CLASS, App.CSS.NOTIFICATIONS);
            final Element divNotifications = ElementU.addElementFirst(html, Html.DIV, null, styleNotifies);
            for (final Alert alert : alertsDisplay) {
                final String severity = alert.getSeverity().toString().toLowerCase(Locale.ENGLISH);
                final String severityClass = Value.join(Html.SPACE, App.CSS.LEVEL, severity);
                final NameTypeValues styleNotify = NTV.create(Html.CLASS, App.CSS.NOTIFICATION);
                final NameTypeValues styleSeverity = NTV.create(Html.CLASS, severityClass);
                final NameTypeValues styleTimestamp = NTV.create(Html.CLASS, App.CSS.TIMESTAMP);
                final NameTypeValues styleText = NTV.create(Html.CLASS, App.CSS.TEXT);
                final Element divNotify = ElementU.addElement(divNotifications, Html.DIV, null, styleNotify);
                if (alert.getLinks() != null) {
                    addLinksTo(divNotify, alert.getLinks());
                }
                if (alert.getActions() != null) {
                    addActionsTo(divNotify, alert.getActions());
                }
                ElementU.addElement(divNotify, Html.SPAN, alert.getIcon(), styleSeverity);
                ElementU.addElement(divNotify, Html.SPAN, locus.toString(alert.getDate()), styleTimestamp);
                ElementU.addElement(divNotify, Html.SPAN, alert.getMessage(), styleText);
                if (alert.getDetail() != null) {
                    ElementU.addElement(divNotify, Html.PRE, alert.getDetail(), styleText);
                }
                //if (Alert.Severity.ERR == alert.getSeverity()) {
                logAlert(alert);
                //}
            }
        }
    }

    private void addLinksTo(final Element div, final AlertLinks links) throws IOException {
        final Element divButtons = ElementU.addElement(div, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.BUTTONS));
        for (final AlertLink alertLink : links.getLinks()) {
            final String label = bundle.getString(alertLink.getSubject());
            final String href = alertLink.getObject();
            ElementU.addElement(divButtons, Html.A, label, NTV.create(
                    Html.HREF, href, Html.TITLE, label, Html.CLASS, App.CSS.ALERT));
        }
    }

    private void addActionsTo(final Element div, final AlertActions actions) throws IOException {
        final Element form = ElementU.addElement(div, Html.FORM, null,
                NTV.create(Html.METHOD, Html.POST, Html.ACTION, Html.EMPTY));
        final Element divButtons = ElementU.addElement(form, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.BUTTONS));
        final String action = App.Action.ALERT;
        final String id = actions.getID();
        for (final String option : actions.getOptions()) {
            final SubmitToken token = SubmitTokenU.create(App.Target.USER_STATE, action, option, id);
            HtmlU.addButton(divButtons, bundle.getString(option), submitID, token.toString(), App.CSS.ALERT, null);
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
