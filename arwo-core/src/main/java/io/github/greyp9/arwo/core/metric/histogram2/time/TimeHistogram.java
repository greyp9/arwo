package io.github.greyp9.arwo.core.metric.histogram2.time;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;

import java.util.Date;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Predicate;

public final class TimeHistogram {
    private final String name;
    private final String metric;
    private final String durationCell;
    private final String durationPage;
    private final String epoch;
    private final long millisCell;
    private final Map<Date, TimeHistogramPage> pages;

    public String getName() {
        return name;
    }

    public String getMetric() {
        return metric;
    }

    public String getDurationCell() {
        return durationCell;
    }

    public String getDurationPage() {
        return durationPage;
    }

    public String getEpoch() {
        return epoch;
    }

    public Map<Date, TimeHistogramPage> getPages() {
        return pages;
    }

    @SuppressWarnings("checkstyle:magicnumber")
    public TimeHistogram(final String... params) {
        this(params[0], params[1], params[2], params[3], params[4]);
    }

    public TimeHistogram(final String name, final String metric,
                         final String durationCell, final String durationPage, final String epoch) {
        this.name = name;
        this.metric = metric;
        this.durationCell = durationCell;
        this.durationPage = durationPage;
        this.epoch = epoch;
        this.millisCell = DurationU.toMillisP(durationCell);
        this.pages = new TreeMap<>();
    }

    public void clear() {
        pages.clear();
    }

    public void add(final Date date, final double amount) {
        final Date datePage = DateU.floor(date, durationPage, XsdDateU.fromXSDZ(epoch));
        final TimeHistogramPage histogramPage = getPage(datePage);
        histogramPage.add(getIndex(datePage, date), amount);
    }

    public void set(final Date date, final double amount) {
        final Date datePage = DateU.floor(date, durationPage, XsdDateU.fromXSDZ(epoch));
        final TimeHistogramPage histogramPage = getPage(datePage);
        histogramPage.set(getIndex(datePage, date), amount);
    }

    public void setIf(final Date date, final double amount, final Predicate<Double> predicate) {
        final Date datePage = DateU.floor(date, durationPage, XsdDateU.fromXSDZ(epoch));
        final TimeHistogramPage histogramPage = getPage(datePage);
        histogramPage.setIf(getIndex(datePage, date), amount, predicate);
    }

    public double[] getBuckets(final Date dateStart, final Date dateEnd) {
        final int size = getIndex(dateStart, dateEnd);
        final Date datePageStart = DateU.floor(dateStart, durationPage, XsdDateU.fromXSDZ(epoch));
        final Date datePageEnd = DateU.floor(dateEnd, durationPage, XsdDateU.fromXSDZ(epoch));
        final int indexPageStart = getIndex(datePageStart, dateStart);
        final int indexPageEnd = getIndex(datePageEnd, dateEnd);

        final double[] buckets = new double[size];
        int i = 0;
        Date datePage = datePageStart;
        while (datePage.compareTo(dateEnd) < 0) {
            final TimeHistogramPage page = getPage(datePage);
            double[] bucketsPage = page.getBuckets();
            final int indexStart = (datePage.equals(datePageStart)) ? indexPageStart : 0;
            final int indexEnd = (datePage.equals(datePageEnd)) ? indexPageEnd : bucketsPage.length;
            System.arraycopy(bucketsPage, indexStart, buckets, i, (indexEnd - indexStart));
            i += (indexEnd - indexStart);
            datePage = incrementPage(datePage, 1);
        }
        return buckets;
    }

    private TimeHistogramPage getPage(final Date datePage) {
        return pages.computeIfAbsent(datePage, p -> new TimeHistogramPage(datePage, durationCell, durationPage));
    }

    private int getIndex(final Date datePage, final Date date) {
        return (int) ((date.getTime() - datePage.getTime()) / millisCell);
    }

    public Date incrementCell(final Date date, final int times) {
        Date dateNew = date;
        for (int i = 0; (i < times); ++i) {
            dateNew = DurationU.add(dateNew, DateU.Const.TZ_GMT, durationCell);
        }
        return dateNew;
    }

    public Date incrementPage(final Date date, final int times) {
        Date dateNew = date;
        for (int i = 0; (i < times); ++i) {
            dateNew = DurationU.add(dateNew, DateU.Const.TZ_GMT, durationPage);
        }
        for (int i = 0; (i > times); --i) {
            dateNew = DurationU.subtract(dateNew, DateU.Const.TZ_GMT, durationPage);
        }
        return dateNew;
    }

    public static class Const {
        public static final String DURATION = "duration";
        public static final String DURATION_LINE = "durationLine";
        public static final String DURATION_PARAGRAPH = "durationParagraph";
        public static final String DURATION_WORD = "durationWord";
        public static final String HISTOGRAM = "histogram";
        public static final String TZ = "tz";
    }
}
