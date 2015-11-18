package io.github.greyp9.arwo.core.xed.session.action;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xml.DocumentU;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

public class SessionSave {
    private final XedSession session;
    private final Bundle bundle;
    private final Alerts alerts;

    public SessionSave(final XedSession session, final Bundle bundle, final Alerts alerts) {
        this.session = session;
        this.bundle = bundle;
        this.alerts = alerts;
    }

    public final void save() throws IOException {
        final byte[] xmlPretty = DocumentU.toXmlPretty(session.getXed().getDocument());
        final File file = session.getFile();
        final File folder = file.getParentFile();
        // ensure parent folder for specified file path
        FileU.ensureFolders(folder);
        // commit
        if (folder.exists() && (folder.isDirectory())) {
            // save new file
            try {
                StreamU.write(file, xmlPretty);
                alert(Alert.Severity.INFO, "document.save", null);
            } catch (FileNotFoundException e) {
                alert(Alert.Severity.ERR, "document.save.error", e.getMessage());
            }
        } else {
            alert(Alert.Severity.ERR, "document.save.error", new FileNotFoundException(file.getPath()).getMessage());
        }
    }

    private void alert(final Alert.Severity severity, final String key, final String detail) {
        final String message = bundle.getString(key);
        alerts.add(new Alert(severity, message, detail));
    }
}
