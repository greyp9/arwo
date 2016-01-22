package io.github.greyp9.arwo.core.alert;

import io.github.greyp9.arwo.core.alert.action.AlertActions;
import io.github.greyp9.arwo.core.alert.link.AlertLinks;
import io.github.greyp9.arwo.core.date.DateU;

import java.util.Date;

public class Alert {
    private final Date date;
    private final Severity severity;
    private final String message;
    private final String detail;
    private final AlertLinks links;
    private final AlertActions actions;

    public final Date getDate() {
        return DateU.copy(date);
    }

    public final Severity getSeverity() {
        return severity;
    }

    public final String getMessage() {
        return message;
    }

    public final String getDetail() {
        return detail;
    }

    public final AlertLinks getLinks() {
        return links;
    }

    public final AlertActions getActions() {
        return actions;
    }

    public Alert(final Severity severity, final String message) {
        this(severity, message, null, null, null);
    }

    public Alert(final Severity severity, final String message, final String detail) {
        this(severity, message, detail, null, null);
    }

    public Alert(final Severity severity, final String message, final AlertLinks links) {
        this(severity, message, null, links, null);
    }

    public Alert(final Severity severity, final String message, final AlertActions actions) {
        this(severity, message, null, null, actions);
    }

    public Alert(final Severity severity, final String message, final String detail,
                 final AlertLinks links, final AlertActions actions) {
        this.date = new Date();
        this.severity = ((severity == null) ? Severity.INFO : severity);
        this.message = message;
        this.detail = detail;
        this.links = links;
        this.actions = actions;
    }

    public final String toString() {
        return String.format("[%s] [%s] %s %s", date, severity, message, detail);  // i18n
    }

    public final String getIcon() {
        String text;
        if (severity == Severity.QUESTION) {
            text = "[?]";  // i18n
        } else if (severity == Severity.INFO) {
            text = "[i]";  // i18n
        } else if (severity == Severity.WARN) {
            text = "[!]";  // i18n
        } else if (severity == Severity.ERR) {
            text = "[X]";  // i18n
        } else {
            text = "[ ]";  // i18n
        }
        return text;
    }

    public enum Severity {
        QUESTION, INFO, WARN, ERR
    }
}
