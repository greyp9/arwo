package io.github.greyp9.arwo.core.file;

import java.io.File;
import java.io.FileFilter;

public final class FileU {

    private FileU() {
    }

    public static File[] listFiles(final File folder) {
        final File[] files = folder.listFiles();
        return ((files == null) ? new File[0] : files);
    }

    public static File[] listFiles(final File folder, final FileFilter filter) {
        final File[] files = folder.listFiles(filter);
        return ((files == null) ? new File[0] : files);
    }

    public static File toFile(final String path) {
        return ((path == null) ? null : new File(path));
    }

    public static boolean delete(final File file) {
        return file.delete();
    }
}
