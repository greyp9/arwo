package io.github.greyp9.arwo.core.alert.model;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.lang.ExceptionU;

import java.io.IOException;
import java.util.Collection;

public class ExceptionModel {
    private final Alerts alerts;

    public ExceptionModel(final Alerts alerts) {
        this.alerts = alerts;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public final void service(final IOException e, final Alert.Severity severity) {
        final Collection<String> messages = ExceptionU.getMessages(e);
        for (final String message : messages) {
            alerts.add(new Alert(severity, message));
        }
    }

/*
    public void service(Throwable t, Alert.Severity severity) {
        Collection<String> messages = ExceptionU.getMessages(t);
        for (String message : messages) {
            alerts.add(new Alert(severity, message));
        }
    }
*/
}
