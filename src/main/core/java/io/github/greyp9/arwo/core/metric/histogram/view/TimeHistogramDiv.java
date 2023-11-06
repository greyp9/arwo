package io.github.greyp9.arwo.core.metric.histogram.view;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.lang.MathU;
import io.github.greyp9.arwo.core.metric.histogram2.time.TimeHistogram;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.util.Calendar;
import java.util.Date;
import java.util.Properties;
import java.util.TimeZone;

import static io.github.greyp9.arwo.core.metric.histogram2.time.TimeHistogram.Const;

public final class TimeHistogramDiv {
    private final TimeHistogram histogram;
    private final String label;
    private final Date dateView;
    private final String durationView;
    private final TimeZone timeZone;
    private final int linesPerParagraph;
    private final int cellsPerLine;
    private final int cellsPerWord;
    private final float scale;

    public TimeHistogramDiv(final TimeHistogram histogram, final String label, final Date date,
                            final Properties initParams, final NameTypeValues args, final float scale) {
        this.histogram = histogram;
        this.label = label;
        this.dateView = date;
        this.durationView = Value.defaultOnEmpty(args.getValue(Const.DURATION), histogram.getDurationPage());
        this.timeZone = TimeZone.getTimeZone(initParams.getProperty(Const.TZ));
        final long millisCell = DurationU.toMillisP(histogram.getDurationCell());
        final long millisWord = DurationU.toMillisP(initParams.getProperty(Const.DURATION_WORD));
        final long millisLine = DurationU.toMillisP(initParams.getProperty(Const.DURATION_LINE));
        final long millisParagraph = DurationU.toMillisP(initParams.getProperty(Const.DURATION_PARAGRAPH));
        this.linesPerParagraph = (int) (millisParagraph / millisLine);
        this.cellsPerLine = (int) (millisLine / millisCell);
        this.cellsPerWord = (int) (millisWord / millisCell);
        this.scale = scale;
    }

    public void addContentTo(final Element html) {
        final Element div = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, Const.HISTOGRAM));
        ElementU.addElement(div, Html.H1, label);
        final Element table = ElementU.addElement(div, Html.TABLE, null, NTV.create());
        addContentTRHead(table);
        addContentTRs(table);
    }

    private void addContentTRHead(final Element table) {
        final Element tr = ElementU.addElement(table, Html.TR, null, NTV.create());
        for (int i = 0; (i < cellsPerLine); i += cellsPerWord) {
            ElementU.addElement(tr, Html.TD, UTF16.NBSP);
            final String durationColumn = DurationU.durationXSD(DurationU.toMillisP(histogram.getDurationCell()) * i);
            ElementU.addElement(tr, Html.TD, "+" + durationColumn, NTV.create(
                    Html.COLSPAN, Integer.toString(cellsPerWord), Html.STYLE, "align: left;"));
        }
    }

    private void addContentTRs(final Element table) {
        final Date dateStartPage = new Date(dateView.getTime() - timeZone.getOffset(dateView.getTime()));
        final Date dateEndPage = DurationU.add(dateStartPage, DateU.Const.TZ_GMT, durationView);
        final double[] buckets = histogram.getBuckets(dateStartPage, dateEndPage);
        final long interval = DurationU.toMillisP(histogram.getDurationCell());
        final long length = buckets.length;
        // paragraph divider state (for readability)
        final Calendar calendar = Calendar.getInstance(timeZone);
        calendar.setTime(dateStartPage);
        int dow = calendar.get(Calendar.DAY_OF_WEEK);
        // iterate through rows
        for (int i = 0; (i < length); i += cellsPerLine) {
            final Element tr = ElementU.addElement(table, Html.TR);
            final String durationRow = DurationU.durationXSD(interval * i);
            final Date dateLine = DurationU.add(dateStartPage, DateU.Const.TZ_GMT, durationRow);
            // row label
            ElementU.addElement(tr, Html.TD, XsdDateU.toXSDwDOW(dateLine, timeZone));
            // iterate through data columns in each line
            for (int j = 0; (j < cellsPerLine); ++j) {
                // row word break (for readability)
                if (((i % cellsPerLine) == 0) && ((j % cellsPerWord) == 0)) {
                    ElementU.addElement(tr, Html.TD, UTF16.NBSP);
                }
                int index = i + j;
                if (index < length) {
                    // render cell data
                    final long l = (long) buckets[index];
                    final int maxShade = 15;
                    final int log = (l == 0) ? 0 : (l == 1) ? 1 : MathU.log(l, scale);
                    final int colorShade = MathU.bound(0, log, maxShade);
                    final String cssClass = String.format("histogram r%d", colorShade);
                    final String title = ((l == 0L) ? null : Long.toString(l));
                    ElementU.addElement(tr, Html.TD, UTF16.NBSP, NTV.create(Html.CLASS, cssClass, Html.TITLE, title));
                }
            }
            // paragraph divider (for readability)
            if (++dow % linesPerParagraph == 2) {  // solution for week = (Monday - Sunday)
                final Element trP = ElementU.addElement(table, Html.TR, null, NTV.create(Html.STYLE, "height: 0.5em;"));
                ElementU.addElement(trP, Html.TD, null/*, NTV.create(Html.STYLE, "height: 0.5em; !important")*/);
            }
        }
    }
}
