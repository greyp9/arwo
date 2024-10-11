package io.github.greyp9.arwo.core.file.meta;

import java.util.Properties;

public class FileMetaData {
    private final String path;
    private final long length;
    private final long lastModified;
    private final long crc;
    private final boolean directory;
    private final Properties properties;

    public final String getPath() {
        return path;
    }

    public final long getLength() {
        return length;
    }

    public final long getLastModified() {
        return lastModified;
    }

    public final long getCrc() {
        return crc;
    }

    public final boolean isDirectory() {
        return directory;
    }

    public final Properties getProperties() {
        return properties;
    }

    public FileMetaData(final String path, final long length, final long lastModified, final boolean directory) {
        this(path, length, lastModified, 0L, directory);
    }

    public FileMetaData(final String path, final long length, final long lastModified,
                        final long crc, final boolean directory) {
        this.path = path;
        this.length = length;
        this.lastModified = lastModified;
        this.crc = crc;
        this.directory = directory;
        this.properties = new Properties();
    }

    //public void setProperty(final String key, final String value) {
    //    properties.setProperty(key, value);
    //}

    public final String toString() {
        return String.format("[%s][%d][%s][%d][%s]", path, length, lastModified, crc, directory);
    }
}
