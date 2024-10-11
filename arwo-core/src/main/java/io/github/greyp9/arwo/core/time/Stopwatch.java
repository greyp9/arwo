package io.github.greyp9.arwo.core.time;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class Stopwatch {
    private final String label;
    private final List<Date> dates;

    public Stopwatch(final String label) {
        this.label = label;
        this.dates = new ArrayList<Date>();
        lap();
    }

    public final void reset() {
        this.dates.clear();
        lap();
    }

    public final long lap() {
        dates.add(new Date());
        return getLast();
    }

    public final long elapsed() {
        return System.currentTimeMillis() - getStart().getTime();
    }

    public final Date getStart() {
        return dates.get(0);
    }

    public final Date getFinish() {
        return dates.get(dates.size() - 1);
    }

    public final long[] getLaps() {
        final int count = Math.max(0, (dates.size() - 1));
        final long[] laps = new long[count];
        for (int i = 0; (i < count); ++i) {
            final Date start = dates.get(i);
            final Date finish = dates.get(i + 1);
            laps[i] = finish.getTime() - start.getTime();
        }
        return laps;
    }

    public final long getLast() {
        final long[] laps = getLaps();
        return ((laps.length == 0) ? 0L : laps[laps.length - 1]);
    }

    public final String toString() {
        final StringBuilder buffer = new StringBuilder();
        buffer.append(String.format("[%s][%s]", label, getStart()));
        final long[] laps = getLaps();
        for (final long lap : laps) {
            buffer.append(' ').append(lap);
        }
        return buffer.toString();
    }
}
