package io.github.greyp9.arwo.core.file;

import io.github.greyp9.arwo.core.value.Value;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

public final class FileU {

    private FileU() {
    }

    public static void ensureFolders(final File folder) {
        if ((folder != null) && (!folder.exists())) {
            ensureFolders(folder.getParentFile());
            mkdir(folder);
        }
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

    public static File toFileIfExists(final String path) {
        File file = null;
        if (!Value.isEmpty(path)) {
            final File filePath = new File(path);
            if (filePath.exists()) {
                file = filePath;
            }
        }
        return file;
    }

    public static String fromFile(final File file) {
        return ((file == null) ? null : file.getAbsolutePath());
    }

    public static boolean move(final File fileSource, final File fileTarget) throws IOException {
        final Path path = Files.move(fileSource.toPath(), fileTarget.toPath(), StandardCopyOption.ATOMIC_MOVE);
        return (path.equals(fileTarget.toPath()));
    }

    public static boolean delete(final File file) {
        return file.delete();
    }

    public static boolean isDirectory(final File file) {
        return (file != null) && (file.isDirectory());
    }

    public static boolean isLink(final File file) {
        final Logger logger = Logger.getLogger(FileU.class.getName());
        boolean isLink = false;
        try {
            final File parentFile = file.getParentFile();
            final File folderCanonical = (parentFile == null) ? null : parentFile.getCanonicalFile();
            final File fileCanonical = (parentFile == null) ? null : new File(folderCanonical, file.getName());
            final File fileCanonicalC = (fileCanonical == null) ? null : fileCanonical.getCanonicalFile();
            final File fileCanonicalA = (fileCanonical == null) ? null : fileCanonical.getAbsoluteFile();
            isLink = (fileCanonical != null) && (!fileCanonicalC.equals(fileCanonicalA));
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }
        return isLink;
    }

    public static File getCanonicalFile(final File file) {
        final Logger logger = Logger.getLogger(FileU.class.getName());
        File fileCanonical = null;
        try {
            final File folderCanonical = file.getParentFile().getCanonicalFile();
            fileCanonical = new File(folderCanonical, file.getName()).getCanonicalFile();
        } catch (IOException e) {
            logger.severe(e.getMessage());

        }
        return fileCanonical;
    }

    public static File getCanonicalFolder(final File folder) throws IOException {
        return folder.getCanonicalFile();
    }

    public static File getNewest(final Collection<File> files) {
        File fileNewest = null;
        for (final File file : files) {
            if (fileNewest == null) {
                fileNewest = file;
            } else {
                fileNewest = ((file.lastModified() > fileNewest.lastModified()) ? file : fileNewest);
            }
        }
        return fileNewest;
    }
}
