package io.github.greyp9.arwo.core.table.html;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.HtmlU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.filter.Filter;
import io.github.greyp9.arwo.core.table.filter.Filters;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.sort.Sort;
import io.github.greyp9.arwo.core.table.sort.Sorts;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.util.Iterator;

public class TableHeaderView {
    private final Table table;
    private final TableContext context;

    public TableHeaderView(final Table table, final TableContext context) {
        this.table = table;
        this.context = context;
    }

    public final void addContentTo(final Element tableHtml, final String title, final boolean openTB) {
        // table header is always added, since there is always a table header row
        final Element thead = ElementU.addElement(tableHtml, Html.THEAD, null,
                NameTypeValuesU.create(Html.CLASS, Html.TABLE));
        // title row
        final Element tr = ElementU.addElement(thead, Html.TR, null, NameTypeValuesU.create(Html.CLASS, "header"));
        final Element th = ElementU.addElement(tr, Html.TH, null,
                NameTypeValuesU.create(Html.COLSPAN, Integer.toString(table.getMetaData().size())));
        final SubmitToken token = new SubmitToken(App.Target.VIEW_STATE, ViewState.Toggle.RIBBON, table.getID());
        // (for sftp view, "title" is fs path, but viewState.getName() should be used for button)
        HtmlU.addButton(th, title, context.getSubmitID(), token.toString(), "header", null, Html.VALUE_2);
        if (openTB) {
            // optionally add extra table controls
            final boolean openTH = context.getViewState().isOpenTH();
            if (openTH) {
                // table controls row
                addRibbonTo(thead, table.getMetaData());
            }
            // table column headers
            addHeaderRowTo(thead, table.getMetaData(), table.getSorts(), openTH);  // table column headers
            // table column controls
            if (openTH) {
                addControlsRowTo(thead, table.getMetaData());  // table column controls
            }
            // table sort/filter state
            addStatusTo(thead, table.getMetaData(), table.getSorts(), table.getFilters());
        }
    }

    private void addRibbonTo(final Element tableHtml, final RowSetMetaData metaData) {
        final String tableID = table.getID();
        final String submitID = context.getSubmitID();
        // row container
        final int width = metaData.size();
        final Element tr = ElementU.addElement(tableHtml, Html.TR, null, NameTypeValuesU.create(
                Html.CLASS, ViewState.Toggle.RIBBON));
        final Element th = ElementU.addElement(tr, Html.TH, null, NameTypeValuesU.create(
                Html.COLSPAN, Integer.toString(width), Html.CLASS, ViewState.Toggle.RIBBON));
        // reset table UI widget
        final SubmitToken tokenReset = new SubmitToken(App.Target.VIEW_STATE, ViewState.Action.RESET, tableID);
        HtmlU.addButton(th, UTF16.TABLE_RESET, submitID, tokenReset.toString(), ViewState.Toggle.RIBBON, null);
        // baseline table UI widget
        final SubmitToken tokenBaseline = new SubmitToken(App.Target.VIEW_STATE, ViewState.Toggle.BASELINE, tableID);
        HtmlU.addButton(th, UTF16.TABLE_BASELINE, submitID, tokenBaseline.toString(), ViewState.Toggle.RIBBON, null);
        // connect table (to table data source) UI widget
        final SubmitToken tokenConnect = new SubmitToken(App.Target.VIEW_STATE, ViewState.Toggle.CONNECT, tableID);
        final boolean isConnected = context.getViewState().isConnected();
        final String textConnect = (isConnected ? UTF16.TABLE_CONNECT : UTF16.TABLE_DISCONNECT);
        HtmlU.addButton(th, textConnect, submitID, tokenConnect.toString(), ViewState.Toggle.RIBBON, null);
        // paging UI widget
        final SubmitToken tokenPage = new SubmitToken(App.Target.VIEW_STATE, ViewState.Toggle.PAGE, table.getID());
        HtmlU.addButton(th, UTF16.TABLE_PAGE, submitID, tokenPage.toString(), ViewState.Toggle.RIBBON, null);
    }

    private void addStatusTo(final Element tableHtml, final RowSetMetaData metaData,
                             final Sorts sorts, final Filters filters) {
        final String sortsText = getSortsText(metaData, sorts);
        final String filtersText = getFiltersText(metaData, filters);
        final boolean displaySorts = (!Value.isEmpty(sortsText));
        final boolean displayFilters = (!Value.isEmpty(filtersText));
        final boolean displayAlways = (!SystemU.isTrue());
        final boolean addRow = (displaySorts || displayFilters || displayAlways);
        if (addRow) {
            final Element tr = ElementU.addElement(tableHtml, Html.TR);
            final Element th = ElementU.addElement(tr, Html.TH, null, NameTypeValuesU.create(
                    Html.COLSPAN, Integer.toString(metaData.size()), Html.CLASS, "state"));
            if (displaySorts) {
                ElementU.addElement(th, Html.SPAN, sortsText);
            }
            if (displayFilters) {
                ElementU.addElement(th, Html.SPAN, filtersText);
            }
            if (displayAlways) {
                ElementU.addElement(th, Html.SPAN, "[]");
            }
        }
    }

    private String getSortsText(final RowSetMetaData metaData, final Sorts sorts) {
        final Bundle bundle = context.getBundle();
        final StringBuilder buffer = new StringBuilder();
        final Iterator<Sort> iterator = sorts.iterator();
        while (iterator.hasNext()) {
            final Sort sort = iterator.next();
            final String label = bundle.getString(TableU.getKey(metaData.getID(), sort.getName()), sort.getName());
            buffer.append(String.format("[%s %s]", toIcon(sort.isAscending()), label));
        }
        return buffer.toString();
    }

    private String getFiltersText(final RowSetMetaData metaData, final Filters filters) {
        final Bundle bundle = context.getBundle();
        final StringBuilder buffer = new StringBuilder();
        final Iterator<Filter> iterator = filters.iterator();
        while (iterator.hasNext()) {
            final Filter filter = iterator.next();
            final String label = bundle.getString(TableU.getKey(metaData.getID(), filter.getName()), filter.getName());
            final Object value = filter.getValue();
            final String valueText = (value == null) ? null : value.toString();
            buffer.append(String.format("[%s %s %s]", label, filter.getOperator(), valueText));
        }
        return buffer.toString();
    }

    private void addHeaderRowTo(
            final Element tableHtml, final RowSetMetaData metaData, final Sorts sorts, final boolean openTH) {
        final Element tr = ElementU.addElement(tableHtml, Html.TR, null, NameTypeValuesU.create(Html.CLASS, "label"));
        final int count = metaData.size();
        for (int i = 0; (i < count); ++i) {
            if (!context.getViewState().getHiddenColumns().contains(metaData.getName(i))) {
                addHeaderColumnTo(tr, metaData, sorts, i, openTH);
            }
        }
    }

    private void addHeaderColumnTo(
            final Element tr, final RowSetMetaData metaData, final Sorts sorts, final int i, final boolean openTH) {
        final String tableID = table.getID();
        final String name = metaData.getName(i);
        final String key = TableU.getKey(metaData.getID(), name);
        final String label = context.getBundle().getString(key, name);
        final String text = Value.join(" ", toIcon(sorts.get(name)), label);
        if (openTH) {
            final Element th = ElementU.addElement(tr, Html.TH, null, NameTypeValuesU.create(Html.CLASS, "label"));
            // sort
            final SubmitToken token = new SubmitToken(App.Target.VIEW_STATE, ViewState.Action.SORT, tableID, name);
            HtmlU.addButton(th, text, context.getSubmitID(), token.toString(), null, null);
        } else {
            ElementU.addElement(tr, Html.TH, text, NameTypeValuesU.create(Html.CLASS, ViewState.Const.COLUMNS));
        }
    }

    private void addControlsRowTo(final Element tableHtml, final RowSetMetaData metaData) {
        final Element tr = ElementU.addElement(tableHtml, Html.TR, null, NameTypeValuesU.create(
                Html.CLASS, ViewState.Const.COLUMNS));
        final int count = metaData.size();
        for (int i = 0; (i < count); ++i) {
            if (!context.getViewState().getHiddenColumns().contains(metaData.getName(i))) {
                addControlsColumnTo(tr, metaData, i);
            }
        }
    }

    private void addControlsColumnTo(final Element tr, final RowSetMetaData metaData, final int i) {
        final String tableID = table.getID();
        final String submitID = context.getSubmitID();
        // column
        final String name = metaData.getName(i);
        final Element th = ElementU.addElement(tr, Html.TH, null, NameTypeValuesU.create(
                Html.CLASS, ViewState.Const.COLUMNS));
/*
        // sort
        SubmitToken tokenSort = new SubmitToken(App.Target.VIEW_STATE, ViewState.Action.SORT, table.getID(), name);
        HtmlU.addButton(th, UTF16.COLUMN_SORT, targetID, tokenSort.toString(), ViewState.Const.COLUMNS, null);
*/
        // filter
        final SubmitToken tokenFilter = new SubmitToken(App.Target.VIEW_STATE, ViewState.Toggle.FILTERS, tableID, name);
        HtmlU.addButton(th, UTF16.COLUMN_FILTER, submitID, tokenFilter.toString(), ViewState.Const.COLUMNS, null);
        // hide
        final SubmitToken tokenHide = new SubmitToken(App.Target.VIEW_STATE, ViewState.Action.HIDE, tableID, name);
        HtmlU.addButton(th, UTF16.COLUMN_HIDE, submitID, tokenHide.toString(), ViewState.Const.COLUMNS, null);
    }

    private String toIcon(final Boolean ascending) {
        String icon = "";
        if (Boolean.TRUE.equals(ascending)) {
            icon = UTF16.ARROW_UP;
        } else if (Boolean.FALSE.equals(ascending)) {
            icon = UTF16.ARROW_DOWN;
        }
        return icon;
    }
}
