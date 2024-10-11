package io.github.greyp9.arwo.core.xed.session.action;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.session.XedSessions;

import java.io.IOException;

public class SessionReload {
    private final XedSessions sessions;
    private final XedSession session;
    private final Bundle bundle;
    private final Alerts alerts;

    public SessionReload(final XedSessions sessions, final XedSession session,
                         final Bundle bundle, final Alerts alerts) {
        this.sessions = sessions;
        this.session = session;
        this.bundle = bundle;
        this.alerts = alerts;
    }

    public final void reload() throws IOException {
        sessions.removeSession(session.getEntry().getContextPath());
        final String message = bundle.getString("document.reload");
        alerts.add(new Alert(Alert.Severity.INFO, message));
    }
}
