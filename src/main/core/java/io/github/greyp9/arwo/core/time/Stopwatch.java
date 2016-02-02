package io.github.greyp9.arwo.core.time;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class Stopwatch {
    private final List<Date> dates;

    public Stopwatch() {
        this.dates = new ArrayList<Date>();
        lap();
    }

    public final long lap() {
        dates.add(new Date());
        return getLast();
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
}
