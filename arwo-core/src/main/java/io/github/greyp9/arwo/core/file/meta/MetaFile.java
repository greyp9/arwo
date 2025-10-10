package io.github.greyp9.arwo.core.file.meta;

import java.io.ByteArrayInputStream;

public class MetaFile {
    private final FileMetaData metaData;
    private final String contentType;
    private final String contentEncoding;
    private final ByteArrayInputStream bis;

    public final FileMetaData getMetaData() {
        return metaData;
    }

    public final String getContentType() {
        return contentType;
    }

    public final String getContentEncoding() {
        return contentEncoding;
    }

    public final ByteArrayInputStream getBytes() {
        bis.reset();
        return bis;
    }

    public MetaFile(final FileMetaData metaData, final String contentType, final ByteArrayInputStream bis) {
        this(metaData, contentType, null, bis);
    }

    public MetaFile(final FileMetaData metaData,
                    final String contentType,
                    final String contentEncoding,
                    final ByteArrayInputStream bis) {
        this.metaData = metaData;
        this.contentType = contentType;
        this.contentEncoding = contentEncoding;
        this.bis = bis;
    }
}
