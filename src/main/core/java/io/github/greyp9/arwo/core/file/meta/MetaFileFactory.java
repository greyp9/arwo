package io.github.greyp9.arwo.core.file.meta;

import io.github.greyp9.arwo.core.io.StreamU;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;

public final class MetaFileFactory {

    private MetaFileFactory() {
    }

    public static MetaFile create(final String path, final long lastModified, final byte[] bytes) {
        final FileMetaData metaData = new FileMetaData(path, bytes.length, lastModified, false);
        return new MetaFile(metaData, null, new ByteArrayInputStream(bytes));
    }

    public static MetaFile createPath(final File file) throws IOException {
        return createCommon(file, file.getAbsolutePath());
    }

    public static MetaFile createName(final File file) throws IOException {
        return createCommon(file, file.getName());
    }

    private static MetaFile createCommon(final File file, final String path) throws IOException {
        MetaFile metaFile;
        try {
            final FileMetaData metaData = new FileMetaData(
                    path, file.length(), file.lastModified(), file.isDirectory());
            metaFile = new MetaFile(metaData, null, new ByteArrayInputStream(StreamU.read(file)));
        } catch (IOException e) {
            metaFile = null;
        }
        return metaFile;
    }
}
