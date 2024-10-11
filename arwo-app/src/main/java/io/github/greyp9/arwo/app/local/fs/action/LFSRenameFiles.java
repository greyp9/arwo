package io.github.greyp9.arwo.app.local.fs.action;

import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.app.local.fs.data.LFSDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.file.action.FilesRename;
import io.github.greyp9.arwo.core.value.NameTypeValues;

import java.io.File;
import java.io.IOException;

/**
 * Invocation of "rename files" action.
 */
public class LFSRenameFiles {
    private final LFSRequest request;
    private final File folderBase;
    private final Alerts alerts;

    public LFSRenameFiles(final LFSRequest request, final File folderBase) {
        this.request = request;
        this.folderBase = folderBase;
        this.alerts = request.getAlerts();
    }

    public final void apply(final NameTypeValues httpArguments) throws IOException {
        final LFSDataSource lfsDataSource = new LFSDataSource(request, folderBase);
        final File folder = lfsDataSource.getFile(request.getPath());
        if (folder.isDirectory()) {
            final String source = httpArguments.getValue("filesRename.filesRenameType.source");
            final String target = httpArguments.getValue("filesRename.filesRenameType.target");
            new FilesRename(folder, source, target, alerts).run();
            //alerts.add(new Alert(Alert.Severity.INFO, String.format(
            // "FOLDER=[%s] SOURCE=[%s] TARGET=[%s]", folder.getAbsolutePath(), source, target)));
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, "Action available only on folders."));
        }
    }
}
