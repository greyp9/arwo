package io.github.greyp9.arwo.core.metric.histogram.test;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogram;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogramSerializer;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogramSerializerMem;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.util.Date;
import java.util.TreeMap;
import java.util.logging.Logger;

@SuppressWarnings("checkstyle:magicnumber")
public class TimeHistogramTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final File folderClass = new File(String.format("./target/%s", getClass().getSimpleName()));

    @Before
    public void setUp() throws Exception {
        io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testCreateSimple() {
        long durationCell = DurationU.toMillisP("P1D");
        long durationPage = DurationU.toMillisP("P31D");
        Date date = DateU.floor(XsdDateU.fromXSDZ("2000-01-01T00:00:00Z"), durationPage);
        TimeHistogram histogram = new TimeHistogram(null, null, null,
                durationCell, durationPage, durationPage, durationPage, durationPage, durationPage);
        logger.finest(histogram.toString());
        for (int a = 0; (a < 3); ++a) {
            final Date dateIt = DurationU.add(date, DateU.Const.TZ_GMT, String.format("P%dD", a));
            histogram.add(dateIt, 1);
        }
        final double[] buckets = histogram.getBuckets(date, 0, histogram.getPageSize());
        Assert.assertEquals(31, buckets.length);
        for (int i = 0; (i < 3); ++i) {
            Assert.assertEquals(1L, (long) buckets[i]);
        }
        for (int j = 3; (j < 31); ++j) {
            Assert.assertEquals(0L, (long) buckets[j]);
        }
        final int pos = 7;
        final int len = histogram.getPageSize() - pos;
        final double[] buckets2 = histogram.getBuckets(date, pos, len);
        for (int j = 0; (j < len); ++j) {
            Assert.assertEquals(0L, (long) buckets2[j]);
        }
    }

    @Test
    public void testAdvance() {
        long durationCell = DurationU.toMillisP("P1D");
        long durationPage = DurationU.toMillisP("P7D");
        Date date = DateU.floor(XsdDateU.fromXSDZ("2000-01-01T00:00:00Z"), durationPage);
        long durationPages = DurationU.toMillisP("P91D");
        final File folder = new File(folderClass, "advance");
        TimeHistogram histogram = new TimeHistogram(null, null, folder.getPath(),
                durationCell, durationCell, durationPage, durationPage, durationPage, durationPages);
        logger.fine(histogram.toString());
        for (int a = 0; (a < 91); ++a) {
            final Date dateIt = DurationU.add(date, DateU.Const.TZ_GMT, String.format("P%dD", a));
            histogram.add(dateIt, a);
        }
/* old concept "advance()" no longer implemented
        histogram.advance(7);
        Assert.assertEquals("2000-01-08T00:00:00Z", XsdDateU.toXSDZ(histogram.getDateStart()));
        final double[] buckets = histogram.getBuckets(0, histogram.getLength());
        for (int i = 0; (i < 84); ++i) {
            Assert.assertEquals(i + 7, (int) buckets[i]);
        }
        for (int j = 84; (j < 91); ++j) {
            Assert.assertEquals(0, (int) buckets[j]);
        }
*/
    }

    @Test
    public void testCreate() {
        final File folder = new File(folderClass, "create");
        TimeHistogram histogram = new TimeHistogram("foo", null, folder.getPath(),
                "PT1M", "PT15M", "PT1H", "PT6H", "P1D", "P5D");
        Assert.assertEquals(DurationU.toMillisP(DurationU.Const.ONE_MINUTE), histogram.getDurationCell());
        Assert.assertEquals(15 * DurationU.toMillisP(DurationU.Const.ONE_MINUTE), histogram.getDurationWord());
        Assert.assertEquals(DurationU.toMillisP(DurationU.Const.ONE_HOUR), histogram.getDurationLine());
        Assert.assertEquals(6 * DurationU.toMillisP(DurationU.Const.ONE_HOUR), histogram.getDurationParagraph());
        Assert.assertEquals(1440, histogram.getPageSize());
        Assert.assertEquals(1440 * 5, histogram.getPageSize() * histogram.getPageCount());
    }

    @Test
    public void testNormalizeAdvance() {
        final File folder = new File(folderClass, "normalize-advance");
        final TimeHistogram histogram = new TimeHistogram("foo", null, folder.getPath(),
                "PT1M", "PT15M", "PT1H", "PT1H", "PT1H", "PT2H");
        logger.finest(histogram.toString());
        final Date dateStart = DateU.floor(new Date(), "PT1H");
        final Date dateEnd = DurationU.add(dateStart, DateU.Const.TZ_GMT, "P1D");
        final long durationIt = DurationU.toMillisP("PT1M");
        Date date = DateU.copy(dateStart);
/* old concept "normalize()" no longer implemented
        while (date.before(dateEnd)) {
            date.setTime(new Date(date.getTime() + durationIt).getTime());
            histogram.normalize(date);
        }
        while (date.after(dateStart)) {
            date.setTime(new Date(date.getTime() - durationIt).getTime());
            histogram.normalize(date);
        }
*/
    }

    @Test
    public void testSerialize() {
        final File folder = new File(folderClass, "serialize");
        final TimeHistogram histogram = new TimeHistogram(
                "foo", null, folder.getPath(), "PT1M", "PT1M", "PT5M", "PT5M", "PT5M", "PT10M");
        logger.finest(histogram.toString());
        final Date floor = DateU.floor(new Date(), "PT5M");
        final Date plusPT1M = DurationU.add(floor, DateU.Const.TZ_GMT, "PT1M");
        histogram.add(floor, 1);
        histogram.add(plusPT1M, 2);
        // serialize
        final TreeMap<String, byte[]> store = new TreeMap<>();
        final TimeHistogramSerializer serializer = new TimeHistogramSerializerMem(store);
        final byte[] bytes = serializer.toBytes(histogram.getHistogramPage(floor));
        // deserialize
        final TimeHistogram histogramCodec = new TimeHistogram(
                "foo", null, ".", "PT1M", "PT1M", "PT5M", "PT5M", "PT5M", "PT10M");
        final TimeHistogramSerializer deserializer = new TimeHistogramSerializerMem(store);
        deserializer.toHistogram(histogramCodec, bytes);
        // check data
        final double[] buckets = histogramCodec.getBuckets(floor, 0, histogramCodec.getPageSize());
        final int length = buckets.length;
        Assert.assertEquals(5, buckets.length);
        Assert.assertEquals(1, (int) buckets[0]);
        Assert.assertEquals(2, (int) buckets[1]);
        for (int i = 2; (i < length); ++i) {
            Assert.assertEquals(0, (int) buckets[i]);
        }
    }
}
