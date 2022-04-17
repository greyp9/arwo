package io.github.greyp9.arwo.core.metric.histogram.test;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogram;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogramPage;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogramSerializer;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogramSerializerFS;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.io.File;
import java.util.Date;
import java.util.Map;
import java.util.logging.Logger;

@SuppressWarnings("checkstyle:magicnumber")
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TimeHistogramSerializerTest {
    private static final Logger LOGGER = Logger.getLogger(TimeHistogramSerializerTest.class.getName());
    private static final File FOLDER_TEST = new File(SystemU.tempDir(), "TimeHistogramSerializerTest");

    @BeforeClass
    public static void beforeClass() {
        final boolean mkdir = FOLDER_TEST.mkdir();
        LOGGER.finest(String.format("CREATE TEMP FOLDER: [%s][%s]", mkdir, FOLDER_TEST.getPath()));
    }

    @AfterClass
    public static void afterClass() {
        new FindInFolderQuery(FOLDER_TEST, "foo.*.xml", false).getFound().forEach(File::delete);
        final boolean delete = FOLDER_TEST.delete();
        LOGGER.finest(String.format("DELETE TEMP FOLDER: [%s][%s]", delete, FOLDER_TEST.getPath()));
    }

    @Test
    public void testSerializer1_CreateHistogram() {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00Z");
        final long durationCell = DurationU.toMillisP("PT5S");
        final long durationPage = DurationU.toMillisP("PT1M");
        final TimeHistogram histogramFoo = new TimeHistogram("foo", null, FOLDER_TEST.getPath(),
                durationCell, durationPage, durationPage, durationPage, durationPage, durationPage);
        for (long offset = 0L; (offset < durationPage); offset += durationCell) {
            final Date dateIt = new Date(date.getTime() + offset);
            histogramFoo.add(dateIt, (double) (offset / durationCell));
        }
        final TimeHistogramSerializer serializer = new TimeHistogramSerializerFS();
        serializer.save(histogramFoo, date);
        Assert.assertTrue(new File(FOLDER_TEST, "foo.2000-01-01T00-00Z.xml").exists());
    }

    @Test
    public void testSerializer2_ReadHistogram() {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00Z");
        final long durationCell = DurationU.toMillisP("PT5S");
        final long durationPage = DurationU.toMillisP("PT1M");
        final int dataPoints = (int) (durationPage / durationCell);
        final TimeHistogram histogramFoo = new TimeHistogram("foo", null, FOLDER_TEST.getPath(),
                durationCell, durationPage, durationPage, durationPage, durationPage, durationPage);
        final TimeHistogramSerializer serializer = new TimeHistogramSerializerFS();
        serializer.load(histogramFoo, date);
        final double[] buckets = histogramFoo.getBuckets(date, 0, dataPoints);
        for (int i = 0; (i < dataPoints); ++i) {
            Assert.assertEquals(buckets[i], i, 0.01);
        }
    }

    @Test
    public void testSerializer3_UpdateHistogram() {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00Z");
        final long durationCell = DurationU.toMillisP("PT5S");
        final long durationPage = DurationU.toMillisP("PT1M");
        final TimeHistogram histogramFoo = new TimeHistogram("foo", null, FOLDER_TEST.getPath(),
                durationCell, durationPage, durationPage, durationPage, durationPage, durationPage);
        final TimeHistogramSerializer serializer = new TimeHistogramSerializerFS();
        serializer.load(histogramFoo, date);
        for (long offset = 0L; (offset < durationPage); offset += durationCell) {
            final Date dateIt = new Date(date.getTime() + offset);
            histogramFoo.add(dateIt, (double) ((durationPage - offset) / durationCell));
        }
        serializer.save(histogramFoo, date);
    }

    @Test
    public void testSerializer4_ReadHistogram() {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00Z");
        final long durationCell = DurationU.toMillisP("PT5S");
        final long durationPage = DurationU.toMillisP("PT1M");
        final int dataPoints = (int) (durationPage / durationCell);
        final TimeHistogram histogramFoo = new TimeHistogram("foo", null, FOLDER_TEST.getPath(),
                durationCell, durationPage, durationPage, durationPage, durationPage, durationPage);
        final TimeHistogramSerializer serializer = new TimeHistogramSerializerFS();
        serializer.load(histogramFoo, date);
        final double[] buckets = histogramFoo.getBuckets(date, 0, dataPoints);
        for (int i = 0; (i < dataPoints); ++i) {
            Assert.assertEquals(buckets[i], 12, 0.01);
        }
    }

    @Test
    public void testSerializer5_AdditionalHistogram() {
        final Date date = XsdDateU.fromXSDZ("2000-01-02T00:00:00Z");
        final long durationCell = DurationU.toMillisP("PT5S");
        final long durationPage = DurationU.toMillisP("PT1M");
        final TimeHistogram histogramFoo = new TimeHistogram("foo", null, FOLDER_TEST.getPath(),
                durationCell, durationPage, durationPage, durationPage, durationPage, durationPage);
        for (long offset = 0L; (offset < durationPage); offset += durationCell) {
            final Date dateIt = new Date(date.getTime() + offset);
            histogramFoo.add(dateIt, (double) (offset / durationCell));
            histogramFoo.add(dateIt, (double) ((durationPage - offset) / durationCell));
        }
        final TimeHistogramSerializer serializer = new TimeHistogramSerializerFS();
        serializer.save(histogramFoo, date);
        Assert.assertTrue(new File(FOLDER_TEST, "foo.2000-01-02T00-00Z.xml").exists());
        Assert.assertEquals(2, new FindInFolderQuery(FOLDER_TEST, "foo.*.xml", false).getFound().size());
    }

    @Test
    public void testSerializer6_UpdateMultipleHistogram() {
        final long durationCell = DurationU.toMillisP("PT5S");
        final long durationPage = DurationU.toMillisP("PT1M");
        final TimeHistogram histogramFoo = new TimeHistogram("foo", null, FOLDER_TEST.getPath(),
                durationCell, durationPage, durationPage, durationPage, durationPage, durationPage);
        final Date date1 = XsdDateU.fromXSDZ("2000-01-01T00:00:00Z");
        for (long offset = 0L; (offset < durationPage); offset += durationCell) {
            final Date dateIt = new Date(date1.getTime() + offset);
            histogramFoo.add(dateIt, (double) (offset / durationCell));
        }
        final Date date2 = XsdDateU.fromXSDZ("2000-01-02T00:00:00Z");
        for (long offset = 0L; (offset < durationPage); offset += durationCell) {
            final Date dateIt = new Date(date2.getTime() + offset);
            histogramFoo.add(dateIt, (double) ((2 * offset) / durationCell));
        }
        final TimeHistogramSerializer serializer = new TimeHistogramSerializerFS();
        serializer.update(histogramFoo);
    }

    @Test
    public void testSerializer7_VerifyState() {
        final long durationCell = DurationU.toMillisP("PT5S");
        final long durationPage = DurationU.toMillisP("PT1M");
        final TimeHistogram histogramFoo = new TimeHistogram("foo", null, FOLDER_TEST.getPath(),
                durationCell, durationPage, durationPage, durationPage, durationPage, durationPage);
        Assert.assertEquals(2, new FindInFolderQuery(FOLDER_TEST, "foo.*.xml", false).getFound().size());
        final TimeHistogramSerializer serializer = new TimeHistogramSerializerFS();
        final Date date1 = XsdDateU.fromXSDZ("2000-01-01T00:00:00Z");
        final Date date2 = XsdDateU.fromXSDZ("2000-01-02T00:00:00Z");
        serializer.load(histogramFoo, date1);
        serializer.load(histogramFoo, date2);
        final Map<Date, TimeHistogramPage> histogramPages = histogramFoo.getHistogramPages();
        Assert.assertTrue(histogramPages.containsKey(date1));
        Assert.assertTrue(histogramPages.containsKey(date2));

        final TimeHistogramPage histogramPage1 = histogramPages.get(date1);
        final double[] buckets1 = histogramPage1.getBuckets(2, 2);
        Assert.assertEquals(2, buckets1.length);
        Assert.assertEquals((12 + 2), buckets1[0], 0.01);

        final TimeHistogramPage histogramPage2 = histogramPages.get(date2);
        final double[] buckets2 = histogramPage2.getBuckets(2, 1);
        Assert.assertEquals(1, buckets2.length);
        Assert.assertEquals((12 + (2 * 2)), buckets2[0], 0.01);
    }
}
