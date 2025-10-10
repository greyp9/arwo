package io.github.greyp9.arwo.core.file.meta;

import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.Http;

import java.util.Properties;

public class FileMetaData {
    private final String path;
    private final long length;
    private final long lastModified;
    private final long crc;
    private final Type type;
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

    public final Type getType() {
        return type;
    }

    public final boolean isDirectory() {
        return Type.DIRECTORY.equals(type);
    }

    public final Properties getProperties() {
        return properties;
    }

    public FileMetaData(final String path, final long length, final long lastModified, final boolean directory) {
        this(path, length, lastModified, 0L, directory);
    }

    public FileMetaData(final String path, final long length, final long lastModified,
                        final long crc, final boolean directory) {
        this(path, length, lastModified, crc, directory ? Type.DIRECTORY : Type.FILE);
    }

    public FileMetaData(final String path, final long length, final long lastModified,
                        final long crc, final Type type) {
        this.path = path;
        this.length = length;
        this.lastModified = lastModified;
        this.crc = crc;
        this.type = type;
        this.properties = new Properties();
    }

    public final String getProperty(final String key) {
        return properties.getProperty(key);
    }

    public final void setProperty(final String key, final String value) {
        properties.setProperty(key, value);
    }

    public final String toString() {
        return String.format("[%s][%d][%s][%d][%s]", path, length, lastModified, crc, type);
    }

    // FileMetaData supplemental attributes
    public static final String PERMS = "perms";
    public static final String OWNER = "owner";
    public static final String GROUP = "group";
    public static final String LINK = "link";

    // FileMetaData classifications
    public enum Type {
        FILE,
        DIRECTORY,
        LINK
    }

    public static String toTypeText(final Type type) {
        String text;
        if (Type.DIRECTORY.equals(type)) {
            text = UTF16.ICON_FOLDER;
        } else if (Type.FILE.equals(type)) {
            text = UTF16.ICON_FILE;
        } else if (Type.LINK.equals(type)) {
            text = UTF16.ICON_SYMLINK;
        } else {
            text = Http.Token.QUERY;
        }
        return text;
    }
}
