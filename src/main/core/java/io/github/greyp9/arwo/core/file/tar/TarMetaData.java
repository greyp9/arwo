package io.github.greyp9.arwo.core.file.tar;

import io.github.greyp9.arwo.core.file.meta.FileMetaData;

public class TarMetaData extends FileMetaData {
    private final String link;

    public TarMetaData(final String path, final long length, final long lastModified,
                       final boolean directory, final String link) {
        super(path, length, lastModified, directory);
        this.link = link;
    }

    public final String getLink() {
        return link;
    }
}
