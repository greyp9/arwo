package io.github.greyp9.arwo.core.xed.session.action;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.file.zip.ZipAppender;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xml.pretty.DocumentPrettyU;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Date;
import java.util.Properties;

public class SessionCommit {
    private final XedSession session;
    private final Bundle bundle;
    private final Alerts alerts;

    public SessionCommit(final XedSession session, final Bundle bundle, final Alerts alerts) {
        this.session = session;
        this.bundle = bundle;
        this.alerts = alerts;
    }

    public final void commit(final String comment, final Properties properties) throws IOException {
        final File file = session.getFile();
        final File folderParent = file.getParentFile();
        final File folder = ((folderParent == null)
                ? new File(SystemU.userDir()) : folderParent);  // SessionSave pattern
        // ensure parent folder for specified file path
        FileU.ensureFolders(folder);
        // commit
        if (folder.exists() && (folder.isDirectory())) {
            // commit new revision
            try {
                commit(comment, file);
                alert(Alert.Severity.INFO, "document.commit", Value.defaultOnEmpty(comment, null));
                properties.setProperty(App.Action.COMMIT, Boolean.FALSE.toString());
            } catch (FileNotFoundException e) {
                alert(Alert.Severity.ERR, "document.commit.error", e.getMessage());
            }
        } else {
            alert(Alert.Severity.ERR, "document.commit.error",
                    new FileNotFoundException(file.getPath()).getMessage());
        }
    }

    private void commit(final String comment, final File file) throws IOException {
        final byte[] xml = DocumentPrettyU.toXmlPretty(session.getXed().getDocument());
        final Date date = new Date();
        final String folderName = DateX.toFilename(date);
        final String name = Value.join(Http.Token.SLASH, folderName, file.getName());
        final FileMetaData metaData = new FileMetaData(name, xml.length, date.getTime(), false);
        final MetaFile metaFile = new MetaFile(metaData, null, new ByteArrayInputStream(xml));
        final File fileRevisions = new File(file.getParentFile(), file.getName() + ".zip");
        final ZipAppender zipAppender = new ZipAppender(fileRevisions);
        zipAppender.append(comment, metaFile);
    }

    private void alert(final Alert.Severity severity, final String key, final String detail) {
        final String message = bundle.getString(key);
        alerts.add(new Alert(severity, message, detail));
    }
}
