package io.github.greyp9.arwo.core.metric.histogram.view;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.metric.histogram2.time.TimeHistogram;
import io.github.greyp9.arwo.core.value.Matrix;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.util.Date;
import java.util.Properties;
import java.util.TimeZone;

public final class TimeHistogramText {
    private final TimeHistogram histogram;
    private final String label;
    private final Date dateView;
    private final String durationView;
    private final TimeZone timeZone;
    private final int linesPerParagraph;
    private final int cellsPerLine;
    private final int cellsPerWord;

    public TimeHistogramText(final TimeHistogram histogram, final String label, final Date date,
                             final Properties initParams, final NameTypeValues args) {
        this.histogram = histogram;
        this.label = label;
        this.dateView = date;
        this.durationView = Value.defaultOnEmpty(
                args.getValue(TimeHistogram.Const.DURATION), histogram.getDurationPage());
        this.timeZone = TimeZone.getTimeZone(initParams.getProperty(TimeHistogram.Const.TZ));
        final long millisCell = DurationU.toMillisP(histogram.getDurationCell());
        final long millisWord = DurationU.toMillisP(initParams.getProperty(TimeHistogram.Const.DURATION_WORD));
        final long millisLine = DurationU.toMillisP(initParams.getProperty(TimeHistogram.Const.DURATION_LINE));
        final long millisParagraph = DurationU.toMillisP(
                initParams.getProperty(TimeHistogram.Const.DURATION_PARAGRAPH));
        this.linesPerParagraph = (int) (millisParagraph / millisLine);
        this.cellsPerLine = (int) (millisLine / millisCell);
        this.cellsPerWord = (int) (millisWord / millisCell);
    }

    public void addContentTo(final Element html) {
        final Element div = ElementU.addElement(html, Html.DIV, null,
                NTV.create(Html.CLASS, TimeHistogram.Const.HISTOGRAM));
        ElementU.addElement(div, Html.H1, label);
        ElementU.addElement(div, Html.PRE, getText());
    }

    public String getText() {
        final Date dateStartPage = new Date(dateView.getTime() - timeZone.getOffset(dateView.getTime()));
        final Date dateEndPage = DurationU.add(dateStartPage, DateU.Const.TZ_GMT, durationView);
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
            final String durationColumn = DurationU.durationXSD(durationCell * column);
            matrix.setColumnLabel(column, "+" + durationColumn);
        }
        // data rows
        for (int row = 0; (row < rowsData); ++row) {
            // row labels
            final String durationRow = DurationU.durationXSD(durationCell * cellsPerLine * row);
            final Date dateLine = DurationU.add(dateStartPage, DateU.Const.TZ_GMT, durationRow);
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
