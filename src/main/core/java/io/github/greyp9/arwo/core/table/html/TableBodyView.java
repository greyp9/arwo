package io.github.greyp9.arwo.core.table.html;

import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.table.cell.Duration;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.sql.Types;
import java.util.Iterator;

public class TableBodyView {
    private final Table table;
    private final TableContext context;

    public TableBodyView(final Table table, final TableContext context) {
        this.table = table;
        this.context = context;
    }

    public final void addContentTo(final Element tableHtml) {
        final Element tbody = ElementU.addElement(tableHtml, Html.TBODY, null,
                NameTypeValuesU.create(Html.CLASS, Html.TABLE));
        if (table.getRows() == 0) {
            addEmptyRowTo(tbody);
        } else {
            addRowsTo(tbody);
        }
    }

    private void addEmptyRowTo(final Element tableHtml) {
        final Element tr = ElementU.addElement(tableHtml, Html.TR);
        final Element th = ElementU.addElement(tr, Html.TH, null, NameTypeValuesU.create(
                Html.COLSPAN, Integer.toString(table.getMetaData().size())));
        ElementU.addElement(th, Html.SPAN, UTF16.NBSP, NameTypeValuesU.create());
    }

    private void addRowsTo(final Element tableHtml) {
        final ViewState viewState = context.getViewState();
        // default page object when disabled
        final Page page = ((viewState.getPage() == null) ? new Page(0, Integer.MAX_VALUE) : viewState.getPage());
        int ordinal = -1;
        final Iterator<Row> iterator = table.iterator();
        // skip rows in previous pages
        for (int i = 0; ((iterator.hasNext()) && (i < page.getPosition())); ++i) {
            iterator.next();
        }
        // add rows in current page
        int count = 0;
        while ((iterator.hasNext()) && (count < page.getCount())) {
            addRowTo(tableHtml, iterator.next(), ++ordinal);
            ++count;
        }
    }

    private void addRowTo(final Element tableHtml, final Row row, final int ordinal) {
        final boolean alt = ((ordinal % Const.GUIDE_ROW_PERIOD) >= Const.GUIDE_ROW_ALT);  // guide row background color
        final Element tr = ElementU.addElement(tableHtml, Html.TR);
        if (row.isHighlight()) {
            ElementU.setAttribute(tr, Html.CLASS, Const.ACTIVE);
        } else if (alt) {
            ElementU.setAttribute(tr, Html.CLASS, Const.ALT);
        }
        final int count = table.getColumns();
        for (int i = 0; (i < count); ++i) {
            final int type = table.getMetaData().getType(i);
            if (!context.getViewState().getHiddenColumns().contains(table.getMetaData().getName(i))) {
                addColumnTo(tr, row, i, type);
            }
        }
    }

    private void addColumnTo(final Element tr, final Row row, final int i, final int type) {
        final Object value = row.getColumn(i);
        if (value instanceof TableViewLink) {
            addCell(tr, (TableViewLink) value);
        } else {
            addCell(tr, value, type);
        }
    }

    private void addCell(final Element tr, final Object value, final int type) {
        final String text = toCellText(value, type);
        final boolean tooBig = (text.length() > Const.MAX_WIDTH_DISPLAY);
        if (tooBig) {
            final String textDisplay = text.substring(0, Const.MAX_WIDTH_DISPLAY) + "...";
            ElementU.addElement(tr, Html.TD, textDisplay, NameTypeValuesU.create(Html.TITLE, text));
        } else {
            final Element td = ElementU.addElement(tr, Html.TD, text, NameTypeValuesU.create());
            addStyle(td, value);
        }
    }

    private String toCellText(final Object value, final int type) {
        String text;
        if (value == null) {
            text = "";
        } else if (value instanceof Boolean) {
            text = toCellText((Boolean) value);
        } else if (type == Types.BOOLEAN) {
            text = toCellText(Boolean.valueOf(value.toString()));
        } else {
            text = value.toString();
        }
        return text;
    }

    private String toCellText(final Boolean value) {
        return ((value ? UTF16.CHECKBOX_TRUE : UTF16.CHECKBOX_FALSE) + " " + value);
    }

    private void addStyle(final Element td, final Object value) {
        if (value instanceof Number) {
            ElementU.setAttribute(td, Html.CLASS, Const.NUMBER);
        } else if (value instanceof Duration) {
            ElementU.setAttribute(td, Html.CLASS, Const.NUMBER);
        }
    }

    private void addCell(final Element tr, final TableViewLink tableViewLink) {
        final Element td = ElementU.addElement(tr, Html.TD, null,
                NameTypeValuesU.create(Html.TITLE, tableViewLink.getTitle()));
        // 'display: block' makes entire cell a hyperlink, not just the inner text
        ElementU.addElement(td, Html.A, tableViewLink.getText(),
                NameTypeValuesU.create(Html.HREF, tableViewLink.getHref()));
    }

    private static class Const {
        private static final int MAX_WIDTH_DISPLAY = 360;
        private static final int GUIDE_ROW_PERIOD = 6;
        private static final int GUIDE_ROW_ALT = 3;

        private static final String ACTIVE = "active";
        private static final String ALT = "alt";
        private static final String NUMBER = "number";
    }
}
