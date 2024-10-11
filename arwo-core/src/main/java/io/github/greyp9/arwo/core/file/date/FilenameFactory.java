package io.github.greyp9.arwo.core.file.date;

import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.value.Value;

import java.io.File;
import java.util.Date;

public final class FilenameFactory {

    private FilenameFactory() {
    }

    public static File getUnused(final String pattern, final Date date) {
        final File filePattern = new File(pattern);
        final File folder = filePattern.getParentFile();
        final String filenamePattern = filePattern.getName();
        final String dateString = Value.defaultOnEmpty(DateX.toFilename(date), "");
        int i = 0;
        String filename = filenamePattern.replace("$DATE", dateString);
        File file = new File(folder, filename);
        while (file.exists()) {
            filename = filenamePattern.replace("$DATE", String.format("%s.%d", dateString, (++i)));
            file = new File(folder, filename);
        }
        return file;
    }

    public static File getUnused(final File folder, final String pattern, final Date date) {
        final String dateString = Value.defaultOnEmpty(DateX.toFilename(date), "");
        int i = 0;
        String filename = pattern.replace("$DATE", dateString);
        File file = new File(folder, filename);
        while (file.exists()) {
            filename = pattern.replace("$DATE", String.format("%s.%d", dateString, (++i)));
            file = new File(folder, filename);
        }
        return file;
    }
}
