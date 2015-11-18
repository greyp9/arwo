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
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedActionCommit;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xml.DocumentU;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Date;
import java.util.Locale;
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

    public final void commit(final NameTypeValues httpArguments, final Properties properties) throws IOException {
        final String comment = new XedActionCommit(Locale.getDefault()).getComment(httpArguments);
        final byte[] xmlPretty = DocumentU.toXmlPretty(session.getXed().getDocument());
        final File file = session.getFile();
        final File folder = file.getParentFile();
        // ensure parent folder for specified file path
        FileU.ensureFolders(folder);
        // commit
        if (folder.exists() && (folder.isDirectory())) {
            // commit new revision
            try {
                final String folderName = DateX.toFilename(new Date(file.lastModified()));
                final String name = Value.join(Http.Token.SLASH, folderName, file.getName());
                final FileMetaData metaData = new FileMetaData(name, xmlPretty.length, new Date().getTime(), false);
                final MetaFile metaFile = new MetaFile(metaData, new ByteArrayInputStream(xmlPretty));
                final File fileRevisions = new File(folder, file.getName() + ".zip");
                final ZipAppender zipAppender = new ZipAppender(fileRevisions);
                zipAppender.append(comment, metaFile);
                alert(Alert.Severity.INFO, "document.commit", Value.defaultOnEmpty(comment, null));
                properties.setProperty(App.Action.COMMIT, Boolean.FALSE.toString());
            } catch (FileNotFoundException e) {
                alert(Alert.Severity.ERR, "document.commit.error", e.getMessage());
            }
        } else {
            alert(Alert.Severity.ERR, "document.commit.error", new FileNotFoundException(file.getPath()).getMessage());
        }
    }

    private void alert(final Alert.Severity severity, final String key, final String detail) {
        final String message = bundle.getString(key);
        alerts.add(new Alert(severity, message, detail));
    }
}
