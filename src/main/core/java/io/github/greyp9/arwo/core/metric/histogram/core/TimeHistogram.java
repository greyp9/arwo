package io.github.greyp9.arwo.core.metric.histogram.core;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.lifecycle.core.Disposable;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public final class TimeHistogram implements Disposable {
    private final String name;
    private final String metric;
    private final String folder;

    private final long durationCell;
    private final long durationWord;
    private final long durationLine;
    private final long durationParagraph;
    private final long durationPage;
    private final long durationPages;

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
        this(name, metric, folder,
                DurationU.toMillisP(durationCell), DurationU.toMillisP(durationWord),
                DurationU.toMillisP(durationLine), DurationU.toMillisP(durationParagraph),
                DurationU.toMillisP(durationPage), DurationU.toMillisP(durationPages));
    }

    @SuppressWarnings("checkstyle:parameternumber")
    public TimeHistogram(final String name, final String metric, final String folder,
                         final long durationCell, final long durationWord, final long durationLine,
                         final long durationParagraph, final long durationPage, final long durationPages) {
        this.name = name;
        this.metric = metric;
        this.folder = folder;
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

    public String getName() {
        return name;
    }

    public String getMetric() {
        return metric;
    }

    public String getFolder() {
        return folder;
    }

    public Map<Date, TimeHistogramPage> getHistogramPages() {
        synchronized (this) {
            return pages;
        }
    }

    public TimeHistogramPage getHistogramPage(final Date date) {
        synchronized (this) {
            return pages.get(date);
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
            final Date dateStart = DateU.floor(date, durationPage);
            final TimeHistogramPage page = pages.computeIfAbsent(dateStart,
                    p -> new TimeHistogramPage(dateStart, durationCell, getPageSize()));
            page.add(date, amount);
        }
    }

    public void set(final Date date, final double amount) {
        synchronized (this) {
            final Date dateStart = DateU.floor(date, durationPage);
            final TimeHistogramPage page = pages.computeIfAbsent(dateStart,
                    p -> new TimeHistogramPage(dateStart, durationCell, getPageSize()));
            page.set(date, amount);
        }
    }

    public void setIf(final Date date, final double amount, final Predicate<Double> predicate) {
        synchronized (this) {
            final Date dateStart = DateU.floor(date, durationPage);
            final TimeHistogramPage page = pages.computeIfAbsent(dateStart,
                    p -> new TimeHistogramPage(dateStart, durationCell, getPageSize()));
            page.setIf(date, amount, predicate);
        }
    }

    public void fillIn(final Date date) {
        synchronized (this) {
            final Date dateStart = DateU.floor(date, durationPage);
            final TimeHistogramPage page = pages.computeIfAbsent(dateStart,
                    p -> new TimeHistogramPage(dateStart, durationCell, getPageSize()));
            page.fillIn();
        }
    }

    public void addPage(final Date date, final TimeHistogramPage page) {
        pages.put(date, page);
    }

    public TimeHistogram diff(final Date date, final String metricDiff) {
        final TimeHistogram histogramDiff = new TimeHistogram(name, metricDiff, folder,
                durationCell, durationWord, durationLine, durationParagraph, durationPage, durationPages);
        synchronized (this) {
            final TimeHistogramPage page = pages.get(date);
            histogramDiff.addPage(date, page.diff());
        }
        return histogramDiff;
    }

    public void expireCache(final Date date) {
        synchronized (this) {
            final Date dateStart = DateU.floor(date, durationPage);
            final List<Date> dates = pages.keySet().stream().filter(
                    dateIt -> !dateIt.equals(dateStart)).collect(Collectors.toList());
            dates.forEach(dateIt -> new TimeHistogramSerializerFS().save(this, dateIt));
            dates.forEach(pages::remove);
        }
    }

    public double[] getBuckets(final Date date, final int pos, final int len) {
        synchronized (this) {
            final Date dateStart = DateU.floor(date, durationPage);
            final TimeHistogramPage page = pages.computeIfAbsent(dateStart,
                    p -> new TimeHistogramPage(dateStart, durationCell, getPageSize()));
            return page.getBuckets(pos, len);
        }
    }

    // get buckets with TZ shift
    public double[] getBuckets(final Date date, final int pos, final int len, final int shift) {
        synchronized (this) {
            final Date dateStart = DateU.floor(date, durationPage);
            final TimeHistogramPage page = pages.computeIfAbsent(dateStart,
                    p -> new TimeHistogramPage(dateStart, durationCell, getPageSize()));
            final double[] buckets = page.getBuckets(pos, len);
            System.arraycopy(buckets, (-1 * shift), buckets, 0, buckets.length - (-1 * shift));
            return buckets;
        }
    }

    /**
     * Call on object destruction to serialize the data, so that it may be recovered on next use.
     */
    @Override
    public void dispose() {
    }
}
