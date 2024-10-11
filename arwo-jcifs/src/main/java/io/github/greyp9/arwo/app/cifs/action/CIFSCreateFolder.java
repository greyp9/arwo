package io.github.greyp9.arwo.app.cifs.action;

import io.github.greyp9.arwo.app.cifs.core.CIFSRequest;
import io.github.greyp9.arwo.app.cifs.data.CIFSDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.lib.jcifs.fs.connection.CIFSConnection;
import jcifs.smb.SmbFile;

import java.io.IOException;

public class CIFSCreateFolder {
    private final CIFSRequest request;
    private final CIFSConnection connection;
    private final Bundle bundle;
    private final Alerts alerts;

    public CIFSCreateFolder(final CIFSRequest request, final CIFSConnection connection) {
        this.request = request;
        this.connection = connection;
        this.bundle = request.getBundle();
        this.alerts = request.getAlerts();
    }

    public final void apply(final NameTypeValues httpArguments) throws IOException {
        final String filename = httpArguments.getValue("folderNew.folderNewType.filename");
        // send remote command

        final FileX fileX = new FileX(Value.join("", request.getPath(), filename));
        final CIFSDataSource source = new CIFSDataSource(request, connection);
        final SmbFile lstat = source.lstat(fileX.getPath());
        if (lstat.exists()) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format(
                    "SFTPHandlerPostMultipart.file.exists", fileX.getPath(), request.getServer())));
        } else {
            source.createDirectory(fileX.getPath());
            alerts.add(new Alert(Alert.Severity.INFO, bundle.format(
                    "SFTPHandlerPostMultipart.folder.target", fileX.getPath())));
        }
    }
}
