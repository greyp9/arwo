package io.github.greyp9.arwo.app.local.fs.action;

import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.app.local.fs.data.LFSDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;

import java.io.File;
import java.io.IOException;

public class LFSCreateFolder {
    private final LFSRequest request;
    private final File folderBase;
    private final Bundle bundle;
    private final Alerts alerts;

    public LFSCreateFolder(final LFSRequest request, final File folderBase) {
        this.request = request;
        this.folderBase = folderBase;
        this.bundle = request.getBundle();
        this.alerts = request.getAlerts();
    }

    public final void apply(final NameTypeValues httpArguments) throws IOException {
        final String filename = httpArguments.getValue("folderNew.folderNewType.filename");
        // send remote command
        final FileX fileX = new FileX(Value.join("", request.getPath(), filename));
        final LFSDataSource source = new LFSDataSource(request, folderBase);

        final File folderTarget = source.exists(fileX.getPath());
        if (folderTarget == null) {
            source.createDirectory(fileX.getPath());
            alerts.add(new Alert(Alert.Severity.INFO, bundle.format(
                    "SFTPHandlerPostMultipart.folder.target", fileX.getPath())));
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format(
                    "SFTPHandlerPostMultipart.file.exists", fileX.getPath(), "")));
        }
    }
}
