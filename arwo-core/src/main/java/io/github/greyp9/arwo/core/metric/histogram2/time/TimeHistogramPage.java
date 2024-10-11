package io.github.greyp9.arwo.core.metric.histogram2.time;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.value.Value;

import java.util.Date;
import java.util.function.Predicate;

public final class TimeHistogramPage {
    private final Date dateStart;
    private final String durationCell;
    private final double[] buckets;

    public TimeHistogramPage(final Date dateStart, final String durationCell, final String durationPage) {
        this.dateStart = dateStart;
        this.durationCell = durationCell;
        final Date dateFinish = DurationU.add(dateStart, DateU.Const.TZ_GMT, durationPage);
        final long millisCell = DurationU.toMillisP(durationCell);
        final long millisPage = Value.defaultOnNull(DurationU.toDuration(dateStart, dateFinish, null), 0L);
        final int length = (int) (millisPage / millisCell);
        this.buckets = new double[length];
    }

    public void add(final int index, final double amount) {
        if (NumberU.inBounds(index, 0, buckets.length - 1)) {
            buckets[index] += amount;
        }
    }

    public void set(final int index, final double amount) {
        setIf(index, amount, (d -> true));
    }

    public void setIf(final int index, final double amount, final Predicate<Double> predicate) {
        if ((NumberU.inBounds(index, 0, buckets.length - 1)) && (predicate.test(buckets[index]))) {
            buckets[index] = amount;
        }
    }

    public Date getDateStart() {
        return dateStart;
    }

    public String getDurationCell() {
        return durationCell;
    }

    public double[] getBuckets() {
        return buckets;
    }
}
