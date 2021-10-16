package io.github.greyp9.arwo.core.file.zip;

import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.file.meta.MetaFileFactory;
import io.github.greyp9.arwo.core.io.StreamU;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

public class ZipAppender {
    private final File fileZip;
    private final Map<String, FileMetaData> entriesProcessed;

    public ZipAppender(final File fileZip) {
        this.fileZip = fileZip;
        this.entriesProcessed = new TreeMap<String, FileMetaData>();
    }

    public final boolean appendZips(final String comment, final File... files) throws IOException {
        boolean success = false;
        final File fileZipNew = new File(fileZip.getParentFile(), fileZip.getName() + ".new.zip");
        try {
            final FileOutputStream fileOutputStream = new FileOutputStream(fileZipNew, false);
            final ZipOutputStream zipOutputStream = new ZipOutputStream(fileOutputStream);
            // heuristic is to always favor newer content, so add new data to zip first
            addNewEntriesZip(zipOutputStream, comment, entriesProcessed, files);
            addExistingEntries(zipOutputStream, entriesProcessed, fileZip);
            zipOutputStream.finish();
            zipOutputStream.close();
            fileOutputStream.close();
            success = fileZipNew.renameTo(fileZip);
        } finally {
            if (fileZipNew.exists()) {
                success |= FileU.delete(fileZipNew);
            }
        }
        return success;
    }

    public final boolean append(final String comment, final MetaFile... files) throws IOException {
        boolean success = false;
        final File fileZipNew = new File(fileZip.getParentFile(), fileZip.getName() + ".new.zip");
        try {
            final FileOutputStream fileOutputStream = new FileOutputStream(fileZipNew, false);
            final ZipOutputStream zipOutputStream = new ZipOutputStream(fileOutputStream);
            // heuristic is to always favor newer content, so add new data to zip first
            addNewEntries(zipOutputStream, comment, files);
            addExistingEntries(zipOutputStream, entriesProcessed, fileZip);
            zipOutputStream.finish();
            zipOutputStream.close();
            fileOutputStream.close();
            success = fileZipNew.renameTo(fileZip);
        } finally {
            if (fileZipNew.exists()) {
                success |= FileU.delete(fileZipNew);
            }
        }
        return success;
    }

    public final boolean append(final String comment, final File... files) throws IOException {
        boolean success = false;
        final File fileZipNew = new File(fileZip.getParentFile(), fileZip.getName() + ".new.zip");
        try {
            final FileOutputStream fileOutputStream = new FileOutputStream(fileZipNew, false);
            final ZipOutputStream zipOutputStream = new ZipOutputStream(fileOutputStream);
            // heuristic is to always favor newer content, so add new data to zip first
            addNewEntries(zipOutputStream, comment, entriesProcessed, files);
            addExistingEntries(zipOutputStream, entriesProcessed, fileZip);
            zipOutputStream.finish();
            zipOutputStream.close();
            fileOutputStream.close();
            success = fileZipNew.renameTo(fileZip);
        } finally {
            if (fileZipNew.exists()) {
                success |= FileU.delete(fileZipNew);
            }
        }
        return success;
    }

    private static void addExistingEntries(
            final ZipOutputStream zipOutputStream, final Map<String, FileMetaData> entriesProcessed,
            final File fileZip) throws IOException {
        if (fileZip.exists()) {
            final ZipFile zipFile = new ZipFile(fileZip);
            try {
                addExistingEntries(zipOutputStream, entriesProcessed, zipFile);
            } finally {
                zipFile.close();
            }
        }
    }

    private static void addExistingEntries(
            final ZipOutputStream zipOutputStream, final Map<String, FileMetaData> entriesProcessed,
            final ZipFile zipFile) throws IOException {
        final Enumeration<? extends ZipEntry> entries = zipFile.entries();
        while (entries.hasMoreElements()) {
            final ZipEntry zipEntryIn = entries.nextElement();
            //final ZipEntry zipEntryOut = new ZipEntry(zipEntryIn);
            if (!entriesProcessed.containsKey(zipEntryIn.getName())) {
                final byte[] bytes = StreamU.read(zipFile.getInputStream(zipEntryIn));
                zipOutputStream.putNextEntry(zipEntryIn);
                zipOutputStream.write(bytes);
                zipOutputStream.closeEntry();
            }
        }
    }

    private static void addNewEntriesZip(
            final ZipOutputStream zipOutputStream, final String comment,
            final Map<String, FileMetaData> entriesProcessed, final File... files) throws IOException {
        for (final File file : files) {
            addNewEntryZip(zipOutputStream, comment, entriesProcessed, file);
        }
    }

    private static void addNewEntries(
            final ZipOutputStream zipOutputStream, final String comment, final MetaFile... files) throws IOException {
        for (final MetaFile file : files) {
            addNewEntry(zipOutputStream, comment, file);
        }
    }

    private static void addNewEntries(
            final ZipOutputStream zipOutputStream, final String comment,
            final Map<String, FileMetaData> entriesProcessed, final File... files) throws IOException {
        for (final File file : files) {
            final MetaFile metaFile = MetaFileFactory.createName(file);
            if (!entriesProcessed.containsKey(metaFile.getMetaData().getPath())) {
                entriesProcessed.put(metaFile.getMetaData().getPath(), metaFile.getMetaData());
                addNewEntry(zipOutputStream, comment, metaFile);
            }
        }
    }

    private static void addNewEntryZip(
            final ZipOutputStream zipOutputStream, final String comment,
            final Map<String, FileMetaData> entriesProcessed, final File file) throws IOException {
        final ZipFile zipFile = new ZipFile(file);
        try {
            final Enumeration<? extends ZipEntry> zipEntries = zipFile.entries();
            while (zipEntries.hasMoreElements()) {
                final ZipEntry zipEntry = zipEntries.nextElement();
                if (!entriesProcessed.containsKey(zipEntry.getName())) {
                    final InputStream is = zipFile.getInputStream(zipEntry);
                    final MetaFile metaFile = ZipVolume.toMetaFile(zipEntry, is);
                    entriesProcessed.put(metaFile.getMetaData().getPath(), metaFile.getMetaData());
                    addNewEntry(zipOutputStream, comment, metaFile);
                }
            }
        } finally {
            zipFile.close();
        }
    }

    private static void addNewEntry(
            final ZipOutputStream zipOutputStream, final String comment, final MetaFile file) throws IOException {
        try {
            final FileMetaData metaData = file.getMetaData();
            final ZipEntry zipEntry = new ZipEntry(metaData.getPath());
            final byte[] bytes = StreamU.read(file.getBytes());
            zipEntry.setSize(metaData.getLength());
            zipEntry.setTime(metaData.getLastModified());
            zipEntry.setComment(comment);
            zipOutputStream.putNextEntry(zipEntry);
            zipOutputStream.write(bytes);
            zipOutputStream.closeEntry();
        } catch (IOException e) {
            final String classname = ZipAppender.class.getName();
            Logger.getLogger(classname).throwing(classname, null, e);
        }

    }

}
