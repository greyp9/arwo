package io.github.greyp9.arwo.core.metric.histogram.core;

import io.github.greyp9.arwo.core.lang.NumberU;

import java.util.Date;
import java.util.function.Predicate;

public final class TimeHistogramPage {
    private final Date dateStart;
    private final long durationCell;
    private final double[] buckets;

    public TimeHistogramPage(final Date dateStart, final long durationCell, final int length) {
        this.dateStart = dateStart;
        this.durationCell = durationCell;
        this.buckets = new double[length];
    }

    public void add(final Date date, final double amount) {
        final long offset = date.getTime() - dateStart.getTime();
        final int i = (int) (offset / durationCell);
        if (NumberU.inBounds(i, 0, buckets.length - 1)) {
            buckets[i] += amount;
        }
    }

    public void set(final Date date, final double amount) {
        setIf(date, amount, (d -> true));
    }

    public void setIf(final Date date, final double amount, final Predicate<Double> predicate) {
        final long offset = date.getTime() - dateStart.getTime();
        final int i = (int) (offset / durationCell);
        if ((NumberU.inBounds(i, 0, buckets.length - 1)) && (predicate.test(buckets[i]))) {
            buckets[i] = amount;
        }
    }

    public void fillIn() {
        for (int i = 1; (i < buckets.length); ++i) {
            if (buckets[i] == 0) {
                buckets[i] = buckets[i - 1];
            }
        }
    }

    public TimeHistogramPage diff() {
        final TimeHistogramPage diff = new TimeHistogramPage(dateStart, durationCell, buckets.length);
        for (int i = 0; (i < (buckets.length - 1)); ++i) {
            diff.buckets[i] = buckets[i + 1] - buckets[i];
        }
        return diff;
    }

    public Date getDateStart() {
        return dateStart;
    }

    public double[] getBuckets(final int pos, final int len) {
        double[] bucketsCopy = new double[len];
        System.arraycopy(buckets, pos, bucketsCopy, 0, len);
        return bucketsCopy;
    }

    public int getPageSize() {
        return buckets.length;
    }
}
