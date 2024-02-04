package io.github.greyp9.arwo.core.table.html;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.HtmlU;
import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.table.baseline.BaselineValue;
import io.github.greyp9.arwo.core.table.cell.Duration;
import io.github.greyp9.arwo.core.table.cell.TableViewButton;
import io.github.greyp9.arwo.core.table.cell.TableViewGlyph;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.cell.TableViewLinks;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.sql.Types;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

@SuppressWarnings({ "PMD.GodClass", "PMD.TooManyMethods" })
public class TableBodyView {
    private final Table table;
    private final TableContext context;

    public TableBodyView(final Table table, final TableContext context) {
        this.table = table;
        this.context = context;
    }

    public final void addContentTo(final Element tableHtml, final boolean openTB) {
        final Element tbody = ElementU.addElement(tableHtml, Html.TBODY, null,
                NameTypeValuesU.create(Html.CLASS, Html.TABLE));
        final boolean tableClosed = (!openTB);
        if (tableClosed) {
            addEmptyRowTo(tbody, null);
        } else if (table.getRows() == 0) {
            addEmptyRowTo(tbody, UTF16.NBSP);
        } else {
            addRowsTo(tbody);
        }
    }

    private void addEmptyRowTo(final Element tableHtml, final String text) {
        final Element tr = ElementU.addElement(tableHtml, Html.TR);
        final Element th = ElementU.addElement(tr, Html.TH, null, NameTypeValuesU.create(
                Html.COLSPAN, Integer.toString(table.getMetaData().size())));
        if (text != null) {
            ElementU.addElement(th, Html.SPAN, text, NameTypeValuesU.create());
        }
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
            ElementU.setAttribute(tr, Html.CLASS, App.CSS.ACTIVE);
        } else if (alt) {
            ElementU.setAttribute(tr, Html.CLASS, App.CSS.ALT);
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
        if (value instanceof BaselineValue) {
            addCell(tr, (BaselineValue) value, type);
        } else if (value instanceof TableViewLink) {
            addCell(tr, (TableViewLink) value);
        } else if (value instanceof TableViewLinks) {
            addCell(tr, (TableViewLinks) value);
        } else if (value instanceof TableViewButton) {
            addCell(tr, (TableViewButton) value);
        } else if (value instanceof TableViewGlyph) {
            addCell(tr, (TableViewGlyph) value);
        } else {
            addCell(tr, value, type);
        }
    }

    private void addCell(final Element tr, final Object value, final int type) {
        final String text = toCellText(value, type);
        final boolean tooBig = (text.length() > Const.MAX_WIDTH_DISPLAY);
        if (tooBig) {
            final String textDisplay = text.substring(0, Const.MAX_WIDTH_DISPLAY) + UTF16.ELLIPSIS;
            ElementU.addElement(tr, Html.TD, textDisplay, NameTypeValuesU.create(Html.TITLE, text));
        } else if (value instanceof Date) {
            final String duration = DurationU.duration((Date) value, context.getDate(), 2);
            ElementU.addElement(tr, Html.TD, text, NameTypeValuesU.create(Html.TITLE, duration));
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
        } else if (value instanceof Date) {
            text = toCellText((Date) value);
        } else if (value instanceof Long) {
            text = toCellText((Long) value);
        } else if (value instanceof Integer) {
            text = toCellText((Integer) value);
        } else if (value instanceof Duration) {
            text = toCellText((Duration) value);
        } else {
            text = value.toString();
        }
        return text;
    }

    private String toCellText(final Boolean value) {
        return ((value ? UTF16.CHECKBOX_TRUE : UTF16.CHECKBOX_FALSE) + Html.SPACE + value);
    }

    private String toCellText(final Date value) {
        return context.getLocus().toString(value);
    }

    private String toCellText(final Long value) {
        return context.getLocus().toString(value);
    }

    private String toCellText(final Integer value) {
        return context.getLocus().toString(value);
    }

    private String toCellText(final Duration value) {
        return DurationU.durationXSD(value.getValue());
    }

    private void addStyle(final Element td, final Object value) {
        if (value instanceof Number) {
            ElementU.setAttribute(td, Html.CLASS, App.CSS.NUMBER);
        } else if (value instanceof Duration) {
            ElementU.setAttribute(td, Html.CLASS, App.CSS.NUMBER);
        }
    }

    private void addCell(final Element tr, final BaselineValue baselineValue, final int type) {
        final Object valueNew = baselineValue.getNew();
        final Object valueOld = baselineValue.getOld();
        final String textNew = toCellText(valueNew, type);
        final String textOld = (valueOld == null) ? null : "[" + toCellText(valueOld, type) + "]";  // i18n internal
        final boolean isNumber = ((valueNew instanceof Number) || (valueOld instanceof Number));
        final String style = Value.join(Html.SPACE, App.CSS.DIFFERENCE, (isNumber ? App.CSS.NUMBER : null));
        final Element td = ElementU.addElement(tr, Html.TD, null, NameTypeValuesU.create(Html.CLASS, style));
        ElementU.addElement(td, Html.SPAN, textNew, NameTypeValuesU.create(Html.CLASS, App.CSS.NEW));
        if (textOld != null) {
            ElementU.addElement(td, Html.SPAN, textOld, NameTypeValuesU.create(Html.CLASS, App.CSS.OLD));
        }
    }

    private void addCell(final Element tr, final TableViewLink tableViewLink) {
        final Element td = ElementU.addElement(tr, Html.TD, null,
                NameTypeValuesU.create(Html.TITLE, tableViewLink.getTitle()));
        // 'display: block' makes entire cell a hyperlink, not just the inner text
        ElementU.addElement(td, Html.A, tableViewLink.getText(),
                NameTypeValuesU.create(Html.HREF, tableViewLink.getHref()));
    }

    private void addCell(final Element tr, final TableViewLinks tableViewLinks) {
        final Element td = ElementU.addElement(tr, Html.TD, null, NameTypeValuesU.create(Html.CLASS, App.CSS.LINKS));
        final List<TableViewLink> links = tableViewLinks.getLinks();
        for (TableViewLink tableViewLink : links) {
            if (tableViewLink.getHref() != null) {
                ElementU.addElement(td, Html.A, tableViewLink.getText(),
                        NameTypeValuesU.create(Html.CLASS, App.CSS.LINKS, Html.HREF, tableViewLink.getHref()));
            }
        }
    }

    private void addCell(final Element tr, final TableViewButton tvb) {
        final Element td = ElementU.addElement(tr, Html.TD);
        HtmlU.addButton(td, tvb.getText(), tvb.getName(), tvb.getValue(), null, null);
    }

    private void addCell(final Element tr, final TableViewGlyph tableViewGlyph) {
        ElementU.addElement(tr, Html.TD, tableViewGlyph.getText(), NameTypeValuesU.create(Html.CLASS, App.CSS.ENHANCE));
    }

    private static class Const {
        private static final int MAX_WIDTH_DISPLAY = 360;
        private static final int GUIDE_ROW_PERIOD = 6;
        private static final int GUIDE_ROW_ALT = 3;
    }
}
