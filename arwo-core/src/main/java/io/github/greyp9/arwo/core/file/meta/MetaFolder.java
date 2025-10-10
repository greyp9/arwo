package io.github.greyp9.arwo.core.file.meta;

import java.util.List;

public final class MetaFolder {
    private final String location;
    private final List<FileMetaData> files;

    public MetaFolder(final String location, final List<FileMetaData> files) {
        this.location = location;
        this.files = files;
    }

    public String getLocation() {
        return location;
    }

    public List<FileMetaData> getFiles() {
        return files;
    }
}
