package io.github.greyp9.arwo.core.file.meta;

public class FileMetaData {
    private final String path;
    private final long length;
    private final long lastModified;
    private final boolean directory;

    public final String getPath() {
        return path;
    }

    public final long getLength() {
        return length;
    }

    public final long getLastModified() {
        return lastModified;
    }

    public final boolean isDirectory() {
        return directory;
    }

    public FileMetaData(final String path, final long length, final long lastModified, final boolean directory) {
        this.path = path;
        this.length = length;
        this.lastModified = lastModified;
        this.directory = directory;
    }

    public final String toString() {
        return String.format("[%s][%d][%s][%s]", path, length, lastModified, directory);
    }
}
