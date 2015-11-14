package io.github.greyp9.arwo.core.alert;

import io.github.greyp9.arwo.core.date.DateU;

import java.util.Date;

public class Alert {
    private final Date date;
    private final Severity severity;
    private final String message;
    private final String detail;

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

    public Alert(final Severity severity, final String message) {
        this(severity, message, null);
    }

    public Alert(final Severity severity, final String message, final String detail) {
        this.date = new Date();
        this.severity = severity;
        this.message = message;
        this.detail = detail;
    }

    public final String toString() {
        return String.format("[%s] [%s] %s %s", date, severity, message, detail);
    }

    public final String getIcon() {
        String text;
        if (severity == Severity.INFO) {
            text = "[i]";
        } else if (severity == Severity.WARN) {
            text = "[!]";
        } else if (severity == Severity.ERR) {
            text = "[X]";
        } else {
            text = "[?]";
        }
        return text;
    }

    public enum Severity {
        INFO, WARN, ERR
    }
}
