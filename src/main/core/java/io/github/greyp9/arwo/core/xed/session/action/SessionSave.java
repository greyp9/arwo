package io.github.greyp9.arwo.core.xed.session.action;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.trigger.XedTrigger;
import io.github.greyp9.arwo.core.xml.DocumentU;
import org.w3c.dom.Document;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

public class SessionSave {
    private final XedRequest request;
    private final XedSession session;
    private final Bundle bundle;
    private final Alerts alerts;

    public SessionSave(final XedRequest request, final Bundle bundle, final Alerts alerts) {
        this.request = request;
        this.session = request.getSession();
        this.bundle = bundle;
        this.alerts = alerts;
    }

    public final void save() throws IOException {
        final Document document = session.getXed().getDocument();
        final Document documentNormal = DocumentU.toDocument(DocumentU.toXml(document));
        final byte[] xmlPretty = DocumentU.toXmlPretty(documentNormal);
        final File file = session.getFile();
        final File folderParent = file.getParentFile();
        final File folder = ((folderParent == null) ? new File(SystemU.userDir()) : folderParent);
        // ensure parent folder for specified file path
        FileU.ensureFolders(folder);
        // commit
        if (folder.exists() && (folder.isDirectory())) {
            // save new file
            try {
                StreamU.write(file, xmlPretty);
                session.setDateModify(null);
                alert(Alert.Severity.INFO, "document.save", null);
                trigger();
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

    private void trigger() {
        final XedTrigger trigger = session.getTrigger();
        if (trigger != null) {
            trigger.onPersist(request.getHttpRequest().getContextPath(), session.getXed());
        }
    }
}
