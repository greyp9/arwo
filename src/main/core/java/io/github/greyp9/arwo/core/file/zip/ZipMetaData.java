package io.github.greyp9.arwo.core.file.zip;

import io.github.greyp9.arwo.core.file.meta.FileMetaData;

public class ZipMetaData extends FileMetaData {
    private final String comment;
    private final long compressedLength;

    public final String getComment() {
        return comment;
    }

    public final long getCompressedLength() {
        return compressedLength;
    }

    public ZipMetaData(final String path, final long length, final long lastModified, final boolean directory,
                       final String comment, final long crc, final long compressedLength) {
        super(path, length, lastModified, crc, directory);
        this.compressedLength = compressedLength;
        this.comment = comment;
    }
}
