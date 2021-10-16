package io.github.greyp9.arwo.core.file.tar;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.date.DateConvertU;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.io.ByteU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.MathU;
import io.github.greyp9.arwo.core.lang.NumberU;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.zip.GZIPInputStream;

public class TarVolume {
    private final File file;
    private final ByteArrayInputStream bis;

    public TarVolume(final File file) throws IOException {
        this.file = file;
        this.bis = null;
    }

    public TarVolume(final ByteArrayInputStream bis) throws IOException {
        this.file = null;
        this.bis = bis;
    }

    // getEntries()

    public final Collection<TarMetaData> getEntries() throws IOException {
        final boolean isFile = (file != null);
        final boolean isBytes = (bis != null);
        final Collection<TarMetaData> entries = new ArrayList<TarMetaData>();
        if (isFile) {
            getEntries(entries, file);
        } else if (isBytes) {
            getEntries(entries, bis);
        }
        return entries;
    }

    // getEntries(File)

    private static void getEntries(
            final Collection<TarMetaData> entries, final File file) throws IOException {
        final InputStream is = new BufferedInputStream(new FileInputStream(file));
        try {
            getEntriesIS(entries, is);
        } finally {
            is.close();
        }
    }

    // getEntries(byte[])

    private static void getEntries(
            final Collection<TarMetaData> entries, final ByteArrayInputStream bis) throws IOException {
        bis.reset();
        getEntriesIS(entries, bis);
    }

    // getEntriesIS()

    private static void getEntriesIS(
            final Collection<TarMetaData> entries, final InputStream is) throws IOException {
        final GZIPInputStream gis = new GZIPInputStream(is);
        try {
            TarMetaData metaData = getNextEntry(gis);
            while (metaData != null) {
                entries.add(metaData);
                skipEntry(gis, metaData);
                metaData = getNextEntry(gis);
            }
        } finally {
            gis.close();
        }
    }

    // getEntry()

    public final MetaFile getEntry(final String entry) throws IOException {
        final boolean isFile = (file != null);
        final boolean isBytes = (bis != null);
        MetaFile metaFile = null;
        if (isFile) {
            metaFile = getEntry(entry, file);
        } else if (isBytes) {
            metaFile = getEntry(entry, bis);
        }
        return metaFile;
    }


    // getEntry(File)

    private static MetaFile getEntry(final String entry, final File file) throws IOException {
        final InputStream is = new BufferedInputStream(new FileInputStream(file));
        try {
            return getEntryIS(entry, is);
        } finally {
            is.close();
        }
    }

    // getEntry(byte[])

    private static MetaFile getEntry(final String entry, final ByteArrayInputStream bis) throws IOException {
        bis.reset();
        return getEntryIS(entry, bis);
    }

    // getEntryIS()

    private static MetaFile getEntryIS(final String entry, final InputStream is) throws IOException {
        MetaFile metaFile = null;
        final GZIPInputStream gis = new GZIPInputStream(is);
        try {
            TarMetaData metaData = getNextEntry(gis);
            while (metaData != null) {
                final String name = metaData.getPath();
                if (name.equals(entry)) {
                    metaFile = loadEntry(gis, metaData);
                    break;
                } else {
                    skipEntry(gis, metaData);
                }
                metaData = getNextEntry(gis);
            }
        } finally {
            gis.close();
        }
        return metaFile;
    }

    // support

    private static TarMetaData getNextEntry(final InputStream is) throws IOException {
        final byte[] header = StreamU.read(is, Const.TAR_BLOCK_SIZE);
        verifyExpectedActual(Const.TAR_BLOCK_SIZE, header.length);
        TarMetaData metaData;
        final byte type = header[Const.OFF_LINK_FLAG];
        if (Const.TYPE_NORMAL_FILE0 == type) {
            metaData = null;
        } else if (Const.TYPE_NORMAL_FILE == type) {
            metaData = getNextEntry(type, null, header);
        } else if (Const.TYPE_SYMLINK == type) {
            metaData = getNextEntry(type, null, header);
        } else if (Const.TYPE_DIRECTORY == type) {
            metaData = getNextEntry(type, null, header);
        } else if (Const.TYPE_LONG_LINK == type) {
            metaData = getNextEntryLongLink(type, header, is);
        } else {
            throw new IOException(String.format("TAR.GZ/ERROR/TYPE==%d)", type));
        }
        return metaData;
    }

    private static TarMetaData getNextEntryLongLink(
            final byte type, final byte[] header, final InputStream is) throws IOException {
        final String fileNameSizeOctal = UTF8Codec.toString(
                ByteU.extract(header, Const.OFF_SIZE, Const.SZ_SIZE)).trim();
        final int fileNameSize = Integer.parseInt(fileNameSizeOctal, NumberU.Const.RADIX_OCTAL);
        final int blockSize = MathU.roundUp(fileNameSize, Const.TAR_BLOCK_SIZE);
        final byte[] headerFileName = StreamU.read(is, blockSize);
        verifyExpectedActual(blockSize, headerFileName.length);
        final String fileName = UTF8Codec.toString(ByteU.extract(headerFileName, 0, fileNameSize)).trim();
        final byte[] header2 = StreamU.read(is, Const.TAR_BLOCK_SIZE);
        verifyExpectedActual(Const.TAR_BLOCK_SIZE, header2.length);
        return getNextEntry(type, fileName, header2);
    }

    private static TarMetaData getNextEntry(final byte type, final String fileNameIn, final byte[] header) {
        final String fileName = (fileNameIn == null)
                ? UTF8Codec.toString(ByteU.extract(header, Const.OFF_NAME, Const.SZ_NAME)).trim() : fileNameIn;
        final String fileSizeOctal = UTF8Codec.toString(ByteU.extract(header, Const.OFF_SIZE, Const.SZ_SIZE)).trim();
        final long fileSize = Long.parseLong(fileSizeOctal, NumberU.Const.RADIX_OCTAL);
        final String lastModifiedOctal = UTF8Codec.toString(
                ByteU.extract(header, Const.OFF_DATE, Const.SZ_DATE)).trim();
        final long lastModifiedSecs = Long.parseLong(lastModifiedOctal, NumberU.Const.RADIX_OCTAL);
        final long lastModified = DateConvertU.fromSeconds(lastModifiedSecs).getTime();
        final boolean directory = (Const.TYPE_DIRECTORY == type);
        final String linkFileName = UTF8Codec.toString(ByteU.extract(header, Const.OFF_LINK, Const.SZ_LINK)).trim();
        return new TarMetaData(fileName, fileSize, lastModified, directory, linkFileName);
    }

    private static MetaFile loadEntry(final InputStream is, final FileMetaData metaData) throws IOException {
        final int length = (int) metaData.getLength();
        final byte[] bytes = StreamU.read(is, length);
        verifyExpectedActual(length, bytes.length);
        final int blockSize = MathU.roundUp(length, Const.TAR_BLOCK_SIZE);
        final long skip = StreamU.skip(is, (blockSize - length));
        verifyExpectedActual((blockSize - length), skip);
        return new MetaFile(metaData, null, new ByteArrayInputStream(bytes));
    }

    private static void skipEntry(final InputStream is, final FileMetaData metaData) throws IOException {
        final int length = (int) metaData.getLength();
        final int blockSize = MathU.roundUp(length, Const.TAR_BLOCK_SIZE);
        final long skip = StreamU.skip(is, blockSize);
        verifyExpectedActual(blockSize, skip);
    }

    private static void verifyExpectedActual(final long expected, final long actual) throws IOException {
        if (expected != actual) {
            throw new IOException(String.format("TAR.GZ/ERROR/(%d!=%d)", expected, actual));
        }
    }

    private static class Const {
        private static final int OFF_NAME = 0;
        private static final int SZ_NAME = 100;
        private static final int OFF_SIZE = 124;
        private static final int SZ_SIZE = 12;
        private static final int OFF_DATE = 136;
        private static final int SZ_DATE = 12;
        private static final int OFF_LINK_FLAG = 156;
        private static final int OFF_LINK = 157;
        private static final int SZ_LINK = 100;

        private static final int TAR_BLOCK_SIZE = 512;

        private static final byte TYPE_NORMAL_FILE0 = 0;
        private static final byte TYPE_NORMAL_FILE = (byte) '0';
        private static final byte TYPE_SYMLINK = (byte) '2';
        private static final byte TYPE_DIRECTORY = (byte) '5';
        private static final byte TYPE_LONG_LINK = (byte) 'L';
    }


    // https://en.wikipedia.org/wiki/Tar_%28computing%29
}
