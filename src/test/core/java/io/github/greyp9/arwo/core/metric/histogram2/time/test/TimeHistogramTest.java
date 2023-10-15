package io.github.greyp9.arwo.core.metric.histogram2.time.test;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.metric.histogram2.time.TimeHistogram;
import org.junit.Assert;
import org.junit.Test;

import java.util.Date;
import java.util.logging.Logger;

public class TimeHistogramTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private static final String EPOCH = "2023-01-01T00:00:00Z";

    private static final int DAYS_IN_WEEK = 7;
    private static final int DAYS_IN_JAN_FEB = 59;

    private static final int VALUE_INITIAL = 3;
    private static final double DELTA = 0.001;

    @Test
    public void testHistogramGetters() {
        final TimeHistogram timeHistogram = new TimeHistogram("A", "count", "PT1H", "P1M", EPOCH);
        logger.finest(timeHistogram.toString());
        Assert.assertEquals("A", timeHistogram.getName());
        Assert.assertEquals("count", timeHistogram.getMetric());
        Assert.assertEquals("PT1H", timeHistogram.getDurationCell());
        Assert.assertEquals("P1M", timeHistogram.getDurationPage());
        Assert.assertEquals(EPOCH, timeHistogram.getEpoch());
    }

    @Test
    public void testHistogramGetPages() {
        final Date dateEpoch = XsdDateU.fromXSDZ(EPOCH);
        final TimeHistogram timeHistogram = new TimeHistogram("B", "count", "P1D", "P7D", EPOCH);

        final Date dateEnd1 = DurationU.add(dateEpoch, DateU.Const.TZ_GMT, timeHistogram.getDurationPage());
        final double[] buckets1 = timeHistogram.getBuckets(dateEpoch, dateEnd1);
        Assert.assertEquals(DAYS_IN_WEEK, buckets1.length);

        final Date dateEnd2 = DurationU.add(dateEnd1, DateU.Const.TZ_GMT, timeHistogram.getDurationPage());
        final double[] buckets2 = timeHistogram.getBuckets(dateEpoch, dateEnd2);
        Assert.assertEquals(DAYS_IN_WEEK * 2, buckets2.length);
    }

    @Test
    public void testHistogramFixedPageSize() {
        final Date dateEpoch = XsdDateU.fromXSDZ(EPOCH);
        final TimeHistogram histogram = new TimeHistogram("C", "count", "P1D", "P7D", EPOCH);
        // add some values, stored in multiple data pages
        final Date dateEnd = histogram.incrementPage(dateEpoch, 2);
        for (Date date = dateEpoch; date.before(dateEnd); date = histogram.incrementCell(date, 1)) {
            final long amount = (date.getTime() - dateEpoch.getTime()) / DurationU.Const.ONE_DAY_MILLIS;
            histogram.add(date, amount);
        }
        // verify values
        final double[] buckets = histogram.getBuckets(dateEpoch, dateEnd);
        Assert.assertEquals(DAYS_IN_WEEK * 2, buckets.length);
        double expected = 0.0;
        for (Double bucket : buckets) {
            Assert.assertEquals(expected, bucket, DELTA);
            ++expected;
        }
    }

    @Test
    public void testHistogramVaryPageSize() {
        final Date dateEpoch = XsdDateU.fromXSDZ(EPOCH);
        final TimeHistogram histogram = new TimeHistogram("C", "count", "P1D", "P1M", EPOCH);
        // add some values, stored in multiple data pages
        final Date dateEnd = histogram.incrementPage(dateEpoch, 2);
        for (Date date = dateEpoch; date.before(dateEnd); date = histogram.incrementCell(date, 1)) {
            final long amount = (date.getTime() - dateEpoch.getTime()) / DurationU.Const.ONE_DAY_MILLIS;
            histogram.add(date, amount);
        }
        // verify values
        final double[] buckets = histogram.getBuckets(dateEpoch, dateEnd);
        Assert.assertEquals(DAYS_IN_JAN_FEB, buckets.length);
        double expected = 0.0;
        for (Double bucket : buckets) {
            Assert.assertEquals(expected, bucket, DELTA);
            ++expected;
        }
    }

    @Test
    public void testHistogramVaryPageSizePartialPage() {
        final Date dateEpoch = XsdDateU.fromXSDZ(EPOCH);
        final TimeHistogram histogram = new TimeHistogram("D", "count", "P1D", "P1M", EPOCH);
        final Date dateStart = histogram.incrementCell(dateEpoch, VALUE_INITIAL);
        // add some values, stored in multiple data pages
        final Date dateEnd = histogram.incrementPage(dateStart, 2);
        for (Date date = dateEpoch; date.before(dateEnd); date = histogram.incrementCell(date, 1)) {
            final long amount = (date.getTime() - dateEpoch.getTime()) / DurationU.Const.ONE_DAY_MILLIS;
            histogram.add(date, amount);
        }
        // verify values
        final double[] buckets = histogram.getBuckets(dateStart, dateEnd);
        Assert.assertEquals(DAYS_IN_JAN_FEB, buckets.length);
        double expected = VALUE_INITIAL;
        for (Double bucket : buckets) {
            Assert.assertEquals(expected, bucket, DELTA);
            ++expected;
        }
    }
}
