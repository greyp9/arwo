package io.github.greyp9.arwo.core.metric.histogram.view;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.metric.histogram2.time.TimeHistogram;
import io.github.greyp9.arwo.core.value.Matrix;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.util.Date;
import java.util.TimeZone;

public final class TimeHistogramText {
    private final TimeHistogram histogram;
    private final String label;
    private final Date date;
    private final TimeZone timeZone;
    private final int linesPerParagraph;
    private final int cellsPerLine;
    private final int cellsPerWord;

    public TimeHistogramText(final TimeHistogram histogram, final String label,
                             final Date date, final TimeZone timeZone) {
        this.histogram = histogram;
        this.label = label;
        this.date = date;
        this.timeZone = timeZone;
        this.linesPerParagraph = (int) DurationU.Const.ONE_WEEK_DAYS;
        this.cellsPerLine = (int) DurationU.Const.ONE_DAY_HOURS;
        final int wordsPerLine = 6;  // divide day into six sections
        this.cellsPerWord = cellsPerLine / wordsPerLine;
    }

    public void addContentTo(final Element html) {
        final Element div = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, "histogram"));
        ElementU.addElement(div, Html.H1, label);
        ElementU.addElement(div, Html.PRE, getText());
    }

    public String getText() {
        final Date dateStartPage = new Date(date.getTime() - timeZone.getOffset(date.getTime()));
        final Date dateEndPage = histogram.incrementPage(dateStartPage, 1);
        final double[] buckets = histogram.getBuckets(dateStartPage, dateEndPage);
        // paragraph divider state (for readability)
        //final Calendar calendar = Calendar.getInstance(timeZone);
        //calendar.setTime(dateStartPage);
        //int dow = calendar.get(Calendar.DAY_OF_WEEK);

        // setup
        final long durationCell = DurationU.toMillisP(histogram.getDurationCell());
        // find starting place of requested data
        // setup container for requested data
        final int rowsData = buckets.length / cellsPerLine; // (histogram.getPageSize() / histogram.getLineSize());
        final Matrix matrix = new Matrix(rowsData, cellsPerLine);
        // column labels (header row)
        for (int column = 0; (column < cellsPerLine); column += cellsPerWord) {
            final String duration = DurationU.durationXSD(durationCell * column);
            matrix.setColumnLabel(column, "+" + duration);
        }
        // data rows
        for (int row = 0; (row < rowsData); ++row) {
            // row labels
            final String duration = DurationU.durationXSD(durationCell * cellsPerLine * row);
            final Date dateLine = DurationU.add(dateStartPage, DateU.Const.TZ_GMT, duration);
            matrix.setRowLabel(row, XsdDateU.toXSDwDOW(dateLine, timeZone));
            // data cells
            for (int column = 0; (column < cellsPerLine); ++column) {
                int cell = /*cursorStart +*/ ((row * cellsPerLine) + column);
                long value = (long) buckets[cell];
                matrix.set(row, column, value);
            }
        }
        return matrix.render("  ", (cellsPerLine * linesPerParagraph));
    }
}
