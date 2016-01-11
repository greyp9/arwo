package io.github.greyp9.arwo.core.file;

import java.io.File;
import java.io.FileFilter;
import java.util.logging.Level;
import java.util.logging.Logger;

public final class FileU {

    private FileU() {
    }

    public static void ensureFolders(final File folder) {
        if (!folder.getParentFile().exists()) {
            ensureFolders(folder.getParentFile());
        }
        ensureFolder(folder);
    }

    public static void ensureFolder(final File folder) {
        if (!folder.exists()) {
            mkdir(folder);
        }
    }

    private static void mkdir(final File folder) {
        final Logger logger = Logger.getLogger(FileU.class.getName());
        Level level;
        try {
            level = (folder.mkdir() ? Level.FINEST : Level.WARNING);
        } catch (SecurityException e) {
            level = Level.SEVERE;
        }
        logger.log(level, folder.getPath());
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

    public static String fromFile(final File file) {
        return ((file == null) ? null : file.getAbsolutePath());
    }

    public static boolean delete(final File file) {
        return file.delete();
    }
}
