package io.github.greyp9.arwo.core.metric.histogram.core;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.lifecycle.core.Disposable;

import java.util.Date;
import java.util.Map;
import java.util.TreeMap;

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

    private Date dateStart;
    private final Map<Date, TimeHistogramPage> pages;

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
        this.length = (int) (durationPage / durationCell);
        this.dateStart = ((dateStart == null) ? DateU.floor(new Date(), durationPage) : DateU.copy(dateStart));
        this.pages = new TreeMap<>();
        this.durationCell = durationCell;
        this.durationWord = durationWord;
        this.durationLine = durationLine;
        this.durationParagraph = durationParagraph;
        this.durationPage = durationPage;
        this.durationPages = durationPages;
        // recover data from previous application invocation
        //new TimeHistogramSerializer(this, new File(folder)).load(dateStart);
    }

/*
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
*/

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

    public Map<Date, TimeHistogramPage> getHistogramPages() {
        synchronized (this) {
            return pages;
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

    public void add(final Date date, final double amount) {
        synchronized (this) {
            dateStart = DateU.floor(date, durationPage);
            final TimeHistogramPage page = pages.computeIfAbsent(dateStart,
                    p -> new TimeHistogramPage(dateStart, durationCell, length));
            page.add(date, amount);
        }
    }

    public double[] getBuckets(final int pos, final int len) {
        synchronized (this) {
            final TimeHistogramPage page = pages.computeIfAbsent(dateStart,
                    p -> new TimeHistogramPage(dateStart, durationCell, length));
            return page.getBuckets(pos, len);
        }
    }

    /**
     * Call on object destruction to serialize the data, so that it may be recovered on next use.
     */
    @Override
    public void dispose() {
    }
}
