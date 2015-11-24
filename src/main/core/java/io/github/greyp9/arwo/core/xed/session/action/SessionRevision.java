package io.github.greyp9.arwo.core.xed.session.action;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.file.zip.ZipVolume;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
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

public class SessionRevision {
    private final XedSessions sessions;
    private final XedSession session;
    private final Bundle bundle;
    private final Alerts alerts;

    public SessionRevision(
            final XedSessions sessions, final XedSession session, final Bundle bundle, final Alerts alerts) {
        this.sessions = sessions;
        this.session = session;
        this.bundle = bundle;
        this.alerts = alerts;
    }

    public final void loadRevision(final SubmitToken token) throws IOException {
        final File file = session.getFile();
        final File fileRevisions = new File(file.getParentFile(), file.getName() + ".zip");
        final ZipVolume zipVolume = new ZipVolume(fileRevisions);
        final MetaFile metaFile = zipVolume.getEntry(token.getObject());
        if (metaFile == null) {
            final String message = bundle.getString("document.load.revision.error");
            alerts.add(new Alert(Alert.Severity.WARN, message));
        } else {
            loadRevision(file, metaFile);
            final String message = bundle.getString("document.load.revision");
            alerts.add(new Alert(Alert.Severity.INFO, message));
        }
    }

    private void loadRevision(final File file, final MetaFile metaFile) throws IOException {
        // existing session
        final XedEntry entry = session.getEntry();
        final String contextPath = entry.getContextPath();
        final Xed xed = session.getXed();
        final Date dateLoad = session.getDateLoad();
        final XedTrigger trigger = session.getTrigger();
        // new session
        final byte[] xmlRevision = StreamU.read(metaFile.getBytes());
        final Document document = DocumentU.toDocument(xmlRevision);
        final Xed xedNew = new Xed(document, xed.getXsdTypes(), xed.getLocale());
        final XedSession sessionNew = new XedSession(entry, xedNew, file, dateLoad, trigger);
        // replace
        sessions.putSession(contextPath, sessionNew);
    }
}
