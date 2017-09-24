package io.github.greyp9.arwo.core.metric.histogram.core;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.lifecycle.core.Disposable;
import io.github.greyp9.arwo.core.metric.histogram.view.TimeHistogramText;

import java.io.File;
import java.util.Arrays;
import java.util.Date;
import java.util.logging.Logger;

public class TimeHistogram implements Disposable {
    private final String name;
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
    public TimeHistogram(String... params) {
        this(params[0], params[1], params[2], params[3], params[4], params[5], params[6]);
    }

    public TimeHistogram(String name, String durationCell, String durationWord, String durationLine,
                         String durationParagraph, String durationPage, String durationPages) {
        this(name, null,
                DurationU.toMillisP(durationCell), DurationU.toMillisP(durationWord),
                DurationU.toMillisP(durationLine), DurationU.toMillisP(durationParagraph),
                DurationU.toMillisP(durationPage), DurationU.toMillisP(durationPages));
    }

    public TimeHistogram(String name, Date dateStart, long durationCell, long durationWord, long durationLine,
                         long durationParagraph, long durationPage, long durationPages) {
        this.name = name;
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
        new TimeHistogramSerializer(this, new File(SystemU.userDir())).load();
    }

    public String getName() {
        return name;
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

    public void advance(int cursor) {
        synchronized (this) {
            // advance date
            final long durationIt = cursor * getDurationCell();
            dateStart.setTime(new Date(dateStart.getTime() + durationIt).getTime());
            // advance bucket data
            if (cursor < 0) {
                System.arraycopy(buckets, 0, buckets, (-1 * cursor), length + cursor);
                Arrays.fill(buckets, 0, (-1 * cursor), 0);
            } else {
                System.arraycopy(buckets, cursor, buckets, 0, length - cursor);
                Arrays.fill(buckets, length - cursor, length, 0);
            }
            new TimeHistogramSerializer(this, new File(SystemU.userDir())).save();
        }
    }

    public void add(final Date date, double amount) {
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
            final Logger logger = Logger.getLogger(getClass().getName());
            while (true) {
                final long offset = date.getTime() - dateStart.getTime();
                final int i = (int) (offset / durationCell);
                if (i < 0) {
                    logger.info("\n" + new TimeHistogramText(this, getName(), (getPageCount() - 1)).getText());
                    advance(-1 * getPageSize());
                } else if (i >= length) {
                    logger.info("\n" + new TimeHistogramText(this, getName(), 0).getText());
                    advance(getPageSize());
                } else {
                    break;
                }
            }
        }
    }

    public double[] getBuckets(int pos, int len) {
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
        final Logger logger = Logger.getLogger(getClass().getName());
        synchronized (this) {
            int pageCount = (int) (durationPages / durationPage);
            for (int page = 0; (page < pageCount); ++page) {
                logger.info(name + "\n" + new TimeHistogramText(this, name, page).getText());
            }
            new TimeHistogramSerializer(this, new File(SystemU.userDir())).save();
        }
    }
}
