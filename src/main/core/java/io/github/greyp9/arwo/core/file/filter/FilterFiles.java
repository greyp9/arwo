package io.github.greyp9.arwo.core.file.filter;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.lang.NumberU;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

public final class FilterFiles {

    private FilterFiles() {
    }

    public static void byAgeMin(final Collection<File> files, final Date date, final String ageMin) {
        final long lastModifiedMax = date.getTime() - NumberU.toLong(DurationU.toMillis(ageMin), 0);
        final Collection<File> filesToRemove = new ArrayList<File>();
        for (final File file : files) {
            if (file.lastModified() > lastModifiedMax) {
                filesToRemove.add(file);
            }
        }
        files.removeAll(filesToRemove);
    }

    public static void byOldest(final Collection<File> files, final int countRemove) {
        final int filesToRetain = Math.max(0, (files.size() - countRemove));
        while (files.size() > filesToRetain) {
            files.remove(FileU.getNewest(files));
        }
    }
}
