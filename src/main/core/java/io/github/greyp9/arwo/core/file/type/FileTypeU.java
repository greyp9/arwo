package io.github.greyp9.arwo.core.file.type;

import java.util.Locale;

public class FileTypeU {

    public static boolean isZip(final String filename) {
        final String[] extensions = { ".zip", ".jar", ".war", };
        boolean isZipFile = false;
        for (String extension : extensions) {
            isZipFile |= filename.toLowerCase(Locale.ENGLISH).endsWith(extension);
        }
        return isZipFile;
    }
}
