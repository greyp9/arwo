package io.github.greyp9.arwo.core.file;

public class FileX {
    private final String path;
    private final int lastSlash;
    private final int lastDot;

    public FileX(final String path) {
        this.path = path.replace(Const.WIN32_PATH_SEPARATOR, Const.SLASH);
        this.lastSlash = path.lastIndexOf(Const.SLASH);
        this.lastDot = path.lastIndexOf(Const.DOT);
    }

    public final String getPath() {
        return path;
    }

    public final String getFolder() {
        return (lastSlash < 0) ? null : path.substring(0, lastSlash);
    }

    public final String getFolderSlash() {
        return (lastSlash < 0) ? null : path.substring(0, lastSlash + Const.SLASH.length());
    }

    public final String getFilename() {
        return (lastSlash < 0) ? path : path.substring(lastSlash + Const.SLASH.length());
    }

    public final String getExtensionDot() {
        return (lastDot <= lastSlash) ? null : path.substring(lastDot);
    }

    public final String getExtension() {
        return (lastDot <= lastSlash) ? null : path.substring(lastDot + Const.DOT.length());
    }

    @SuppressWarnings("PMD.LongVariable")
    private static class Const {
        private static final String DOT = ".";  // i18n
        private static final String SLASH = "/";  // i18n
        private static final String WIN32_PATH_SEPARATOR = "\\";  // i18n
    }
}
