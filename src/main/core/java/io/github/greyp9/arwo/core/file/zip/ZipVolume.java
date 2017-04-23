package io.github.greyp9.arwo.core.file.zip;

import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.io.StreamU;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

public class ZipVolume {
    private final File file;
    private final ByteArrayInputStream bis;

    public ZipVolume(final File file) throws IOException {
        this.file = file;
        this.bis = null;
    }

    public ZipVolume(final ByteArrayInputStream bis) throws IOException {
        this.file = null;
        this.bis = bis;
    }

    // getEntries()

    public final Collection<ZipMetaData> getEntries() throws IOException {
        final boolean isFile = (file != null);
        final boolean isBytes = (bis != null);
        final Collection<ZipMetaData> entries = new ArrayList<ZipMetaData>();
        if (isFile) {
            getEntries(entries, file);
        } else if (isBytes) {
            getEntries(entries, bis);
        }
        return entries;
    }

    // getEntries(File)

    private static void getEntries(
            final Collection<ZipMetaData> entries, final File file) throws IOException {
        final ZipFile zipFile = new ZipFile(file);
        try {
            getEntries(entries, zipFile);
        } finally {
            zipFile.close();
        }
    }

    private static void getEntries(
            final Collection<ZipMetaData> entries, final ZipFile zipFile) throws IOException {
        final Enumeration<? extends ZipEntry> zipEntries = zipFile.entries();
        while (zipEntries.hasMoreElements()) {
            final ZipEntry zipEntry = zipEntries.nextElement();
            entries.add(toZipMetaData(zipEntry));
        }
    }

    // getEntries(byte[])

    private static void getEntries(
            final Collection<ZipMetaData> entries, final ByteArrayInputStream bis) throws IOException {
        bis.reset();
        final ZipInputStream zis = new ZipInputStream(new BufferedInputStream(bis));
        try {
            getEntries(entries, zis);
        } finally {
            zis.close();
        }
    }

    private static void getEntries(
            final Collection<ZipMetaData> entries, final ZipInputStream zis) throws IOException {
        ZipEntry zipEntry = zis.getNextEntry();
        while (zipEntry != null) {
            entries.add(toZipMetaData(zipEntry));
            zipEntry = zis.getNextEntry();
        }
    }

    // getEntry()

    public final MetaFile getEntry(final String name) throws IOException {
        final boolean isFile = (file != null);
        final boolean isBytes = (bis != null);
        MetaFile metaFile = null;
        if (isFile) {
            metaFile = getEntry(name, file);
        } else if (isBytes) {
            metaFile = getEntry(name, bis);
        }
        return metaFile;
    }

    // getEntry(File)

    private static MetaFile getEntry(final String name, final File file) throws IOException {
        final ZipFile zipFile = new ZipFile(file);
        try {
            return getEntry(name, zipFile);
        } finally {
            zipFile.close();
        }
    }

    private static MetaFile getEntry(final String name, final ZipFile zipFile) throws IOException {
        MetaFile metaFile = null;
        final Enumeration<? extends ZipEntry> zipEntries = zipFile.entries();
        while (zipEntries.hasMoreElements()) {
            final ZipEntry zipEntry = zipEntries.nextElement();
            final String nameIt = zipEntry.getName();
            if (nameIt.equals(name)) {
                metaFile = toMetaFile(zipEntry, zipFile.getInputStream(zipEntry));
                break;
            }
        }
        return metaFile;
    }

    // getEntry(byte[])

    private static MetaFile getEntry(final String name, final ByteArrayInputStream bis) throws IOException {
        bis.reset();
        final ZipInputStream zis = new ZipInputStream(new BufferedInputStream(bis));
        try {
            return getEntry(name, zis);
        } finally {
            zis.close();
        }
    }

    private static MetaFile getEntry(final String name, final ZipInputStream zis) throws IOException {
        MetaFile metaFile = null;
        ZipEntry zipEntry = zis.getNextEntry();
        while (zipEntry != null) {
            final String nameIt = zipEntry.getName();
            if (nameIt.equals(name)) {
                metaFile = toMetaFile(zipEntry, zis);
                break;
            } else {
                StreamU.skipPartial(zis);
            }
            zipEntry = zis.getNextEntry();
        }
        return metaFile;
    }

    // utilities

    public static MetaFile toMetaFile(final ZipEntry zipEntry, final InputStream is) throws IOException {
        // skip ZipEntry header (which may contain inaccurate info) by reading stream
        final byte[] bytesEntry = StreamU.readPartial(is);
        final long length = bytesEntry.length;
        final FileMetaData metaData = new FileMetaData(zipEntry.getName(), length, zipEntry.getTime(), false);
        return new MetaFile(metaData, null, new ByteArrayInputStream(bytesEntry));
    }

    private static ZipMetaData toZipMetaData(final ZipEntry zipEntry) {
        final String name = zipEntry.getName();
        final long size = zipEntry.getSize();
        final long lastModified = zipEntry.getTime();
        final boolean directory = zipEntry.isDirectory();
        final String comment = zipEntry.getComment();
        final long compressedSize = zipEntry.getCompressedSize();
        final long crc = zipEntry.getCrc();
        return new ZipMetaData(name, size, lastModified, directory, comment, crc, compressedSize);
    }
}
