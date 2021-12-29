package io.github.greyp9.arwo.core.metric.histogram.core;

import io.github.greyp9.arwo.core.date.DateU;

import java.util.Date;
import java.util.Map;

public final class TimeHistogramSerializerMem extends TimeHistogramSerializer {
    private final Map<String, byte[]> store;

    public TimeHistogramSerializerMem(final Map<String, byte[]> store) {
        this.store = store;
    }

    @Override
    public void save(final TimeHistogram histogram, final Date date) {
        final Date dateStart = DateU.floor(date, histogram.getDurationPage());
        final TimeHistogramPage page = histogram.getHistogramPage(dateStart);
        store.put(getFile(histogram, dateStart), toBytes(page));
    }

    @Override
    public void load(final TimeHistogram histogram, final Date date) {
        final Date dateStart = DateU.floor(date, histogram.getDurationPage());
        final String file = getFile(histogram, dateStart);
        final byte[] bytes = store.get(file);
        if (bytes != null) {
            toHistogram(histogram, bytes);
        }
    }
}
