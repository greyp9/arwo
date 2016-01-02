package io.github.greyp9.arwo.core.xed.session.action;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.session.XedSession;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

public class SessionValidate {
    private final XedSession session;
    private final Bundle bundle;
    private final Alerts alerts;

    public SessionValidate(final XedSession session, final Bundle bundle, final Alerts alerts) {
        this.session = session;
        this.bundle = bundle;
        this.alerts = alerts;
    }

    public final void validate() throws IOException {
        final Collection<String> messages = session.getXed().validate();
        if (messages.isEmpty()) {
            final String message = bundle.getString("document.validate.no.errors");
            alerts.add(new Alert(Alert.Severity.INFO, message));
        } else {
            final String message = bundle.getString("document.validate.errors");
            final String detail = Value.joinList(Http.Token.CRLF, new ArrayList<Object>(messages));
            alerts.add(new Alert(Alert.Severity.WARN, message, detail, null));
        }
    }
}
