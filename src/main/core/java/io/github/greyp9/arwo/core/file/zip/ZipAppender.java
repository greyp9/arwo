package io.github.greyp9.arwo.core.file.zip;

import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.io.StreamU;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

public class ZipAppender {
    private final File fileZip;

    public ZipAppender(final File fileZip) {
        this.fileZip = fileZip;
    }

    public final boolean append(final String comment, final MetaFile... files) throws IOException {
        boolean success = false;
        final File fileZipNew = new File(fileZip.getParentFile(), fileZip.getName() + ".new.zip");
        try {
            final FileOutputStream fileOutputStream = new FileOutputStream(fileZipNew, false);
            final ZipOutputStream zipOutputStream = new ZipOutputStream(fileOutputStream);
            addExistingEntries(zipOutputStream, fileZip);
            addNewEntries(zipOutputStream, comment, files);
            zipOutputStream.finish();
            zipOutputStream.close();
            fileOutputStream.close();
            success = fileZipNew.renameTo(fileZip);
        } finally {
            if (fileZipNew.exists()) {
                success |= fileZipNew.delete();
            }
        }
        return success;
    }

    private static void addExistingEntries(
            final ZipOutputStream zipOutputStream, final File fileZip) throws IOException {
        if (fileZip.exists()) {
            final ZipFile zipFile = new ZipFile(fileZip);
            try {
                addExistingEntries(zipOutputStream, zipFile);
            } finally {
                zipFile.close();
            }
        }
    }

    private static void addExistingEntries(
            final ZipOutputStream zipOutputStream, final ZipFile zipFile) throws IOException {
        final Enumeration<? extends ZipEntry> entries = zipFile.entries();
        while (entries.hasMoreElements()) {
            final ZipEntry zipEntryIn = entries.nextElement();
            //final ZipEntry zipEntryOut = new ZipEntry(zipEntryIn);
            final byte[] bytes = StreamU.read(zipFile.getInputStream(zipEntryIn));
            zipOutputStream.putNextEntry(zipEntryIn);
            zipOutputStream.write(bytes);
            zipOutputStream.closeEntry();
        }
    }

    private static void addNewEntries(
            final ZipOutputStream zipOutputStream, final String comment, final MetaFile... files) throws IOException {
        for (final MetaFile file : files) {
            addNewEntry(zipOutputStream, comment, file);
        }
    }

    private static void addNewEntry(
            final ZipOutputStream zipOutputStream, final String comment, final MetaFile file) throws IOException {
        final FileMetaData metaData = file.getMetaData();
        final ZipEntry zipEntry = new ZipEntry(metaData.getPath());
        final byte[] bytes = StreamU.read(file.getBytes());
        zipEntry.setSize(metaData.getLength());
        zipEntry.setTime(metaData.getLastModified());
        zipEntry.setComment(comment);
        zipOutputStream.putNextEntry(zipEntry);
        zipOutputStream.write(bytes);
        zipOutputStream.closeEntry();
    }
}
