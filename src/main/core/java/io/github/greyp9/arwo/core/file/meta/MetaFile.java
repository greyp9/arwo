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
}
