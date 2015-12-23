package io.github.greyp9.arwo.app.local.fs.data;

import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.model.ExceptionModel;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.io.StreamU;

import java.io.File;
import java.io.IOException;

public class LFSDataSource {
    private final LFSRequest request;
    private final File folderRoot;

    public LFSDataSource(final LFSRequest request, final File folderRoot) {
        this.request = request;
        this.folderRoot = folderRoot;
    }

    public final File getFile(final String path) {
        return new File(folderRoot, path);
    }

    public final File[] listFiles(final String path) {
        return FileU.listFiles(new File(folderRoot, path));
    }

    public final File exists(final String path) {
        final File file = new File(folderRoot, path);
        return (file.exists() ? file : null);
    }

    public final void write(final byte[] bytes, final String folder, final String filename) throws IOException {
        try {
            final File folderTarget = new File(folderRoot, folder);
            final File fileTarget = new File(folderTarget, filename);
            StreamU.write(fileTarget, bytes);
        } catch (IOException e) {
            new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
        }
    }
}
