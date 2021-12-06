package io.github.greyp9.arwo.core.metric.histogram.core;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.lifecycle.core.Disposable;

import java.io.File;
import java.util.Arrays;
import java.util.Date;

public final class TimeHistogram implements Disposable {
    private final String name;
    private final String metric;
    private final String folder;
    private final int length;

    private final long durationCell;
    private final long durationWord;
    private final long durationLine;
    private final long durationParagraph;
    private final long durationPage;
    private final long durationPages;

    private final Date dateStart;
    private final double[] buckets;

    // for instantiation via reflection
    @SuppressWarnings("checkstyle:magicnumber")
    public TimeHistogram(final String... params) {
        this(params[0], params[1], params[2], params[3], params[4], params[5], params[6], params[7], params[8]);
    }

    @SuppressWarnings("checkstyle:parameternumber")
    public TimeHistogram(final String name, final String metric, final String folder,
                         final String durationCell, final String durationWord, final String durationLine,
                         final String durationParagraph, final String durationPage, final String durationPages) {
        this(name, metric, folder, DateU.floor(new Date(), DurationU.toMillisP(durationPage)),
                DurationU.toMillisP(durationCell), DurationU.toMillisP(durationWord),
                DurationU.toMillisP(durationLine), DurationU.toMillisP(durationParagraph),
                DurationU.toMillisP(durationPage), DurationU.toMillisP(durationPages));
    }

    @SuppressWarnings("checkstyle:parameternumber")
    public TimeHistogram(final String name, final String metric, final String folder, final Date dateStart,
                         final long durationCell, final long durationWord, final long durationLine,
                         final long durationParagraph, final long durationPage, final long durationPages) {
        this.name = name;
        this.metric = metric;
        this.folder = folder;
        this.length = (int) (durationPages / durationCell);
        this.dateStart = ((dateStart == null) ? DateU.floor(new Date(), durationPage) : dateStart);
        this.buckets = new double[length];
        this.durationCell = durationCell;
        this.durationWord = durationWord;
        this.durationLine = durationLine;
        this.durationParagraph = durationParagraph;
        this.durationPage = durationPage;
        this.durationPages = durationPages;
        // recover data from previous application invocation
        new TimeHistogramSerializer(this, new File(folder)).load(dateStart);
    }

    public TimeHistogram(final TimeHistogram histogram, final Date dateStart, final int pos, final int len) {
        this.name = histogram.getName();
        this.metric = histogram.getMetric();
        this.folder = histogram.getFolder();
        this.length = len;
        this.dateStart = DateU.copy(dateStart);
        this.buckets = new double[length];
        final double[] bucketsIn = histogram.getBuckets(pos, len);
        System.arraycopy(bucketsIn, 0, this.buckets, 0, len);
        this.durationCell = histogram.durationCell;
        this.durationWord = histogram.durationWord;
        this.durationLine = histogram.durationLine;
        this.durationParagraph = histogram.durationParagraph;
        this.durationPage = histogram.durationPage;
        this.durationPages = histogram.durationPages;
    }

    public String getName() {
        return name;
    }

    public String getMetric() {
        return metric;
    }

    public String getFolder() {
        return folder;
    }

    public int getLength() {
        return length;
    }

    public Date getDateStart() {
        synchronized (this) {
            return dateStart;
        }
    }

    public void setDateStart(final Date date) {
        synchronized (this) {
            dateStart.setTime(date.getTime());
        }
    }

    public long getDurationCell() {
        return durationCell;
    }

    public long getDurationWord() {
        return durationWord;
    }

    public long getDurationLine() {
        return durationLine;
    }

    public long getDurationParagraph() {
        return durationParagraph;
    }

    public long getDurationPage() {
        return durationPage;
    }

    public long getDurationPages() {
        return durationPages;
    }

    public int getWordSize() {
        return (int) (durationWord / durationCell);
    }

    public int getLineSize() {
        return (int) (durationLine / durationCell);
    }

    public int getParagraphSize() {
        return (int) (durationParagraph / durationLine);
    }

    public int getPageSize() {
        return (int) (durationPage / durationCell);
    }

    public int getPageCount() {
        return (int) (durationPages / durationPage);
    }

    public void advance(final int cursor) {
        synchronized (this) {
            // adjust bucket data
            if (cursor < 0) {
                System.arraycopy(buckets, 0, buckets, (-1 * cursor), length + cursor);
                Arrays.fill(buckets, 0, (-1 * cursor), 0);
            } else {
                // archive records that have been aged out
                TimeHistogram histogramDiscard = new TimeHistogram(this, dateStart, 0, cursor);
                new TimeHistogramSerializer(histogramDiscard, new File(folder)).save(dateStart);
                // normalize data slots
                System.arraycopy(buckets, cursor, buckets, 0, length - cursor);
                Arrays.fill(buckets, length - cursor, length, 0);
            }
            // adjust date
            final long durationIt = cursor * getDurationCell();
            dateStart.setTime(new Date(dateStart.getTime() + durationIt).getTime());
            new TimeHistogramSerializer(this, new File(folder)).save(dateStart);
        }
    }

    public void add(final Date date, final double amount) {
        synchronized (this) {
            final long offset = date.getTime() - dateStart.getTime();
            final int i = (int) (offset / durationCell);
            if (NumberU.inBounds(i, 0, length - 1)) {
                buckets[i] += amount;
            }
        }
    }

    public void normalize(final Date date) {
        synchronized (this) {
            //final Logger logger = Logger.getLogger(getClass().getName());
            while (true) {
                final long offset = date.getTime() - dateStart.getTime();
                final int i = (int) (offset / durationCell);
                if (i < 0) {
                    //logger.info("\n" + new TimeHistogramText(this, getName(), (getPageCount() - 1)).getText());
                    advance(-1 * getPageSize());
                } else if (i >= length) {
                    //logger.info("\n" + new TimeHistogramText(this, getName(), 0).getText());
                    advance(getPageSize());
                } else {
                    break;
                }
            }
        }
    }

    public double[] getBuckets(final int pos, final int len) {
        double[] bucketsCopy = new double[len];
        synchronized (this) {
            System.arraycopy(buckets, pos, bucketsCopy, 0, len);
        }
        return bucketsCopy;
    }

    /**
     * Call on object destruction to serialize the data, so that it may be recovered on next use.
     */
    @Override
    public void dispose() {
        //final Logger logger = Logger.getLogger(getClass().getName());
        synchronized (this) {
            //int pageCount = (int) (durationPages / durationPage);
            //for (int page = 0; (page < pageCount); ++page) {
            //    logger.info(name + "\n" + new TimeHistogramText(this, name, page).getText());
            //}
            new TimeHistogramSerializer(this, new File(folder)).save(dateStart);
        }
    }
}
