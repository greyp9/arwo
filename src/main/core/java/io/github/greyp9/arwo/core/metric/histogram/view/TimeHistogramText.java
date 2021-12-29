package io.github.greyp9.arwo.core.metric.histogram.view;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogram;
import io.github.greyp9.arwo.core.value.Matrix;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.util.Date;

public final class TimeHistogramText {
    private final TimeHistogram histogram;
    private final String label;
    private final Date date;
    private final int page;

    public TimeHistogramText(final TimeHistogram histogram, final String label, final Date date, final int page) {
        this.histogram = histogram;
        this.label = label;
        this.date = date;
        this.page = page;
    }

    public void addContentTo(final Element html) {
        final Element div = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, "histogram"));
        ElementU.addElement(div, Html.H1, label);
        ElementU.addElement(div, Html.PRE, getText());
    }

    public String getText() {
        // setup
        final long durationCell = histogram.getDurationCell();
        // find starting place of requested data
        final int cursorStart = page * histogram.getPageSize();
        final String durationToStartPage = DurationU.durationXSD(durationCell * cursorStart);
        final Date dateStartPage = DurationU.add(date, DateU.Const.TZ_GMT, durationToStartPage);
        final double[] buckets = histogram.getBuckets(date, cursorStart, histogram.getPageSize());
        // setup container for requested data
        final int columnsData = histogram.getLineSize();
        final int rowsData = (histogram.getPageSize() / histogram.getLineSize());
        final Matrix matrix = new Matrix(rowsData, columnsData);
        // column labels (header row)
        for (int column = 0; (column < columnsData); column += histogram.getWordSize()) {
            final String duration = DurationU.durationXSD(durationCell * column);
            matrix.setColumnLabel(column, "+" + duration);
        }
        // data rows
        for (int row = 0; (row < rowsData); ++row) {
            // row labels
            final String duration = DurationU.durationXSD(histogram.getDurationLine() * row);
            final Date dateLine = DurationU.add(dateStartPage, DateU.Const.TZ_GMT, duration);
            matrix.setRowLabel(row, XsdDateU.toXSDZ(dateLine));
            // data cells
            for (int column = 0; (column < columnsData); ++column) {
                int cell = /*cursorStart +*/ ((row * columnsData) + column);
                long value = (long) buckets[cell];
                matrix.set(row, column, value);
            }
        }
        return matrix.render("  ", histogram.getParagraphSize());
    }
}
