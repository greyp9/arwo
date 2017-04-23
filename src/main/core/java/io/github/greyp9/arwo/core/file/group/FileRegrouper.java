package io.github.greyp9.arwo.core.file.group;

import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.TreeMap;

public class FileRegrouper {
    private final Long intervalMillis;
    private final Map<Date, Collection<File>> groupings;

    public FileRegrouper(final String interval) {
        this.intervalMillis = DurationU.toMillis(interval);
        this.groupings = new TreeMap<Date, Collection<File>>();
    }

    public Map<Date, Collection<File>> getGroupings() {
        return groupings;
    }

    public void add(final File file) {
        final Long interval = Value.defaultOnNull(intervalMillis, 0L);
        if (interval > 0) {
            final String name = file.getName();
            final Date date = DateX.fromFilenameMM(name);
            if (date != null) {
                final long offset = date.getTime() % intervalMillis;
                final Date dateGroup = new Date(date.getTime() - offset);
                Collection<File> files = groupings.get(dateGroup);
                if (files == null) {
                    files = new ArrayList<File>();
                    groupings.put(dateGroup, files);
                }
                files.add(file);
            }
        }
    }
}
