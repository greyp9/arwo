package io.github.greyp9.arwo.core.table.html;

import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.io.IOException;

public class TableViewInner {
    private final Table table;
    private final TableContext context;

    public TableViewInner(final Table table, final TableContext context) {
        this.table = table;
        this.context = context;
    }

    public final void addContentTo(final Element html) throws IOException {
        final Element div = ElementU.addElement(html, Html.DIV, null, NameTypeValuesU.create(
                Html.CLASS, Html.TABLE));
        new TableFilterStripView(context).addContentTo(div);
        final Element form = ElementU.addElement(div, Html.FORM, null, NameTypeValuesU.create(
                Html.ACTION, "", Html.METHOD, Html.POST));
        final Element tableHtml = ElementU.addElement(form, Html.TABLE, null, NameTypeValuesU.create(
                Html.CLASS, context.getTableClass(), Html.SUMMARY, getSummary()));
        final boolean openTB = context.getViewState().isOpenTB();
        new TableHeaderView(table, context).addContentTo(tableHtml, getTitle(), openTB);
        new TableFooterView(table, context).addContentTo(tableHtml, openTB);
        new TableBodyView(table, context).addContentTo(tableHtml, openTB);
    }

    public final String getTitle() {
        return ((table.getTitle() == null) ? getTitleNN() : table.getTitle());
    }

    private String getTitleNN() {
        final Bundle bundle = context.getBundle();
        final String value = bundle.getString(TableU.getKey(table.getID(), null), null);
        return (value == null) ? bundle.getString(table.getID()) : value;
    }

    public final String getSummary() {
        return ((table.getSummary() == null) ? getTitle() : table.getSummary());
    }
}
