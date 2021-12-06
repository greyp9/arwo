package io.github.greyp9.arwo.core.metric.histogram.view;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.lang.MathU;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogram;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.util.Date;

public final class TimeHistogramDiv {
    private final TimeHistogram histogram;
    private final String label;
    private final int position;
    private final int linesPerParagraph;
    private final int cellsPerLine;
    private final int cellsPerWord;
    private final float scale;

    public TimeHistogramDiv(final TimeHistogram histogram, final String label, final int position, final float scale) {
        this.histogram = histogram;
        this.label = label;
        this.position = position;
        this.linesPerParagraph = histogram.getParagraphSize();
        this.cellsPerLine = histogram.getLineSize();
        this.cellsPerWord = histogram.getWordSize();
        this.scale = scale;
    }

    public void addContentTo(final Element html) {
        final Element div = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, "histogram"));
        ElementU.addElement(div, Html.H1, label);
        final Element table = ElementU.addElement(div, Html.TABLE, null, NTV.create());
        addContentTRHead(table);
        addContentTRs(table);
    }

    private void addContentTRHead(final Element table) {
        final Element tr = ElementU.addElement(table, Html.TR, null, NTV.create());
        for (int i = 0; (i < cellsPerLine); i += cellsPerWord) {
            ElementU.addElement(tr, Html.TD, UTF16.NBSP);
            final String duration = DurationU.durationXSD(histogram.getDurationCell() * i);
            ElementU.addElement(tr, Html.TD, "+" + duration, NTV.create(
                    Html.COLSPAN, Integer.toString(cellsPerWord), Html.STYLE, "align: left;"));
        }
    }

    private void addContentTRs(final Element table) {
        final int cursorStart = position * histogram.getPageSize();
        final int cursorEnd = cursorStart + histogram.getPageSize();
        final Date dateStart = histogram.getDateStart();
        final double[] buckets = histogram.getBuckets(cursorStart, (cursorEnd - cursorStart));
        final long interval = histogram.getDurationCell();
        final long length = buckets.length;
        final String durationToPage = DurationU.durationXSD(interval * cursorStart);
        final Date dateStartPage = DurationU.add(dateStart, DateU.Const.TZ_GMT, durationToPage);
        // paragraph divider state (for readability)
        int lineInParagraph = 0;
        // iterate through rows
        for (int i = 0; (i < length); i += cellsPerLine) {
            final Element tr = ElementU.addElement(table, Html.TR);
            final String duration = DurationU.durationXSD(interval * i);
            final Date dateLine = DurationU.add(dateStartPage, DateU.Const.TZ_GMT, duration);
            // row label
            ElementU.addElement(tr, Html.TD, XsdDateU.toXSDZ(dateLine));
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
                    final int colorShade = MathU.bound(0, MathU.log(l, scale), maxShade);
                    final String cssClass = String.format("histogram r%d", colorShade);
                    final String title = ((l == 0L) ? null : Long.toString(l));
                    ElementU.addElement(tr, Html.TD, UTF16.NBSP, NTV.create(Html.CLASS, cssClass, Html.TITLE, title));
                }
            }
            // paragraph divider (for readability)
            if (++lineInParagraph % linesPerParagraph == 0) {
                final Element trP = ElementU.addElement(table, Html.TR, null, NTV.create(Html.STYLE, "height: 0.5em;"));
                ElementU.addElement(trP, Html.TD, null/*, NTV.create(Html.STYLE, "height: 0.5em; !important")*/);
            }
        }
    }
}
