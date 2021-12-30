package io.github.greyp9.arwo.core.metric.histogram.test;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogram;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Date;
import java.util.logging.Logger;

public class TimeHistogramExternalLoadTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        logger.entering(getClass().getName(), null);
    }

    @Test
    public void testHistogram_Fabricate_OnePage() {
        final String metric = "foo";
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00Z");
        final long durationCell = DurationU.toMillisP("PT1S");
        final long durationPage = DurationU.toMillisP("PT10S");
        final long range = durationPage / 2;
        final TimeHistogram histogram = new TimeHistogram(null, metric, null,
                durationCell, durationPage, durationPage, durationPage, durationPage, durationPage);
        for (long offset = 0L; (offset < range); offset += durationCell) {
             histogram.add(new Date(date.getTime() + offset), offset);
        }
        final double[] buckets = histogram.getBuckets(date, 0, histogram.getPageSize());
        Assert.assertEquals(4_000, buckets[4], 0.1d);
    }

    @Test
    public void testHistogram_Fabricate_MultiPage() {
        final String metric = "foo";
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00Z");
        final long durationCell = DurationU.toMillisP("PT1S");
        final long durationPage = DurationU.toMillisP("PT10S");
        final long range = durationPage * 2;
        final TimeHistogram histogram = new TimeHistogram(null, metric, null,
                durationCell, durationPage, durationPage, durationPage, durationPage, durationPage);
        for (long offset = 0L; (offset < range); offset += durationCell) {
            final Date dateIt = new Date(date.getTime() + offset);
            histogram.add(dateIt, offset);
        }
        final Date dateNextPage = new Date(date.getTime() + durationPage);
        final double[] buckets = histogram.getBuckets(dateNextPage, 0, histogram.getPageSize());
        Assert.assertEquals(14_000, buckets[4], 0.1d);
    }
}
