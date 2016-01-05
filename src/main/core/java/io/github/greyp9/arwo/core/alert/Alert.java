package io.github.greyp9.arwo.core.alert;

import io.github.greyp9.arwo.core.alert.action.AlertActions;
import io.github.greyp9.arwo.core.date.DateU;

import java.util.Date;

public class Alert {
    private final Date date;
    private final Severity severity;
    private final String message;
    private final String detail;
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

    public final AlertActions getActions() {
        return actions;
    }

    public Alert(final String message) {
        this(Severity.INFO, message, null, null);
    }

    public Alert(final Severity severity, final String message) {
        this(severity, message, null, null);
    }

    public Alert(final Severity severity, final String message, final AlertActions actions) {
        this(severity, message, null, actions);
    }

    public Alert(final Severity severity, final String message, final String detail, final AlertActions actions) {
        this.date = new Date();
        this.severity = severity;
        this.message = message;
        this.detail = detail;
        this.actions = actions;
    }

    public final String toString() {
        return String.format("[%s] [%s] %s %s", date, severity, message, detail);
    }

    public final String getIcon() {
        String text;
        if (severity == Severity.QUESTION) {
            text = "[?]";
        } else if (severity == Severity.INFO) {
            text = "[i]";
        } else if (severity == Severity.WARN) {
            text = "[!]";
        } else if (severity == Severity.ERR) {
            text = "[X]";
        } else {
            text = "[ ]";
        }
        return text;
    }

    public enum Severity {
        QUESTION, INFO, WARN, ERR
    }
}
