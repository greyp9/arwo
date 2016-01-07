package io.github.greyp9.arwo.app.ssh.sftp.action;

import ch.ethz.ssh2.SFTPv3FileAttributes;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;

import java.io.IOException;

public class SFTPCreateFolder {
    private final SFTPRequest request;
    private final SSHConnection connection;
    private final Bundle bundle;
    private final Alerts alerts;

    public SFTPCreateFolder(final SFTPRequest request, final SSHConnection connection) {
        this.request = request;
        this.connection = connection;
        this.bundle = request.getBundle();
        this.alerts = request.getAlerts();
    }

    public final void apply(final NameTypeValues httpArguments) throws IOException {
        final String filename = httpArguments.getValue("folderNew.folderNewType.filename");
        // send remote command

        final FileX fileX = new FileX(Value.join("", request.getPath(), filename));
        final SFTPDataSource source = new SFTPDataSource(request, connection);
        final SFTPv3FileAttributes lstatParent = source.exists(request.getPath());
        final SFTPv3FileAttributes lstat = source.exists(fileX.getPath());
        if (lstat == null) {
            source.createDirectory(fileX.getPath(), lstatParent.permissions);
            alerts.add(new Alert(Alert.Severity.INFO, bundle.format(
                    "SFTPHandlerPostMultipart.folder.target", fileX.getPath())));
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format(
                    "SFTPHandlerPostMultipart.file.exists", fileX.getPath(), request.getServer())));
        }
    }
}
