package io.github.greyp9.arwo.core.xed.session.action;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.session.XedEntry;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.session.XedSessions;
import io.github.greyp9.arwo.core.xed.trigger.XedTrigger;
import io.github.greyp9.arwo.core.xml.DocumentU;
import org.w3c.dom.Document;

import java.io.File;
import java.io.IOException;
import java.util.Date;

public class SessionPretty {
    private final XedSessions sessions;
    private final XedSession session;
    private final Bundle bundle;
    private final Alerts alerts;

    public SessionPretty(final XedSessions sessions, final XedSession session,
                         final Bundle bundle, final Alerts alerts) {
        this.sessions = sessions;
        this.session = session;
        this.bundle = bundle;
        this.alerts = alerts;
    }

    public final void pretty() throws IOException {
        // existing session
        final XedEntry entry = session.getEntry();
        final String contextPath = entry.getContextPath();
        final Xed xed = session.getXed();
        final File file = session.getFile();
        final Date dateLoad = session.getDateLoad();
        final XedTrigger trigger = session.getTrigger();
        // do this step to fix namespaces for new elements (something in JRE?)
        final Document documentNormal = DocumentU.toDocument(DocumentU.toXml(xed.getDocument()));
        final Document document = DocumentU.toDocument(DocumentU.toXmlPretty(documentNormal));
        // new session
        final Xed xedNew = new Xed(document, xed.getXsdTypes(), xed.getLocale());
        final XedSession sessionNew = new XedSession(entry, xedNew, file, dateLoad, trigger);
        // replace
        sessions.putSession(contextPath, sessionNew);
        // user alert
        final String message = bundle.getString("document.pretty");
        alerts.add(new Alert(Alert.Severity.INFO, message));
    }
}
