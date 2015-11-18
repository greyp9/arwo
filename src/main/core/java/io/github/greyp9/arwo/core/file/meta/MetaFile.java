package io.github.greyp9.arwo.core.file.meta;

import java.io.ByteArrayInputStream;

public class MetaFile {
    private final FileMetaData metaData;
    private final ByteArrayInputStream bis;

    public final FileMetaData getMetaData() {
        return metaData;
    }

    public final ByteArrayInputStream getBytes() {
        return bis;
    }

    public MetaFile(final FileMetaData metaData, final ByteArrayInputStream bis) {
        this.metaData = metaData;
        this.bis = bis;
    }

    public static final class Factory {
        private Factory() {
        }

        public static MetaFile create(
                final String path, final long lastModified, final byte[] bytes) {
            final FileMetaData metaData = new FileMetaData(path, bytes.length, lastModified, false);
            return new MetaFile(metaData, new ByteArrayInputStream(bytes));
        }
    }
}
