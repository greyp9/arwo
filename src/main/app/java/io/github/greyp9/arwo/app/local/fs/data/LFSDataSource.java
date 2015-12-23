package io.github.greyp9.arwo.app.local.fs.data;

import io.github.greyp9.arwo.core.file.FileU;

import java.io.File;

public class LFSDataSource {
    private final File folder;

    public LFSDataSource(final File folder) {
        this.folder = folder;
    }

    public final File getFile(final String path) {
        return new File(folder, path);
    }

    public final File[] listFiles(final String path) {
        return FileU.listFiles(new File(folder, path));
    }
}
