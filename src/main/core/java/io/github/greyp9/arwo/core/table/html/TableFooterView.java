package io.github.greyp9.arwo.core.table.html;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.HtmlU;
import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.util.Properties;

public class TableFooterView {
    private final Table table;
    private final TableContext context;

    public TableFooterView(final Table table, final TableContext context) {
        this.table = table;
        this.context = context;
    }

    public final void addContentTo(final Element tableHtml) {
        final Element tfoot = ElementU.addElement(tableHtml, Html.TFOOT, null,
                NameTypeValuesU.create(Html.CLASS, Html.TABLE));
        addFooterRowTo(tfoot);
        if (context.getViewState().getPage() != null) {
            addFooterPageRowTo(tfoot, context.getViewState().getPage());
        }
        ElementU.detachOnEmpty(tfoot);

    }

    private void addFooterRowTo(final Element tfoot) {
        final Properties properties = table.getProperties();
        final String footerL = properties.getProperty(Table.Const.FOOTER_L);
        final String footerC = properties.getProperty(Table.Const.FOOTER_C);
        final String footerR = properties.getProperty(Table.Const.FOOTER_R);
        if ((footerC != null) || (footerL != null) || (footerR != null)) {
            final Element tr = ElementU.addElement(tfoot, Html.TR);
            final Element th = ElementU.addElement(tr, Html.TH, null, NameTypeValuesU.create(
                    Html.COLSPAN, Integer.toString(table.getMetaData().size()), Html.CLASS, "status"));
            addFooterTo(th, "left", footerL, properties.getProperty(Table.Const.FOOTER_HREF_L));
            addFooterTo(th, "center", footerC, properties.getProperty(Table.Const.FOOTER_HREF_C));
            addFooterTo(th, "right", footerR, properties.getProperty(Table.Const.FOOTER_HREF_R));
        }
    }

    private void addFooterTo(final Element th, final String spanClass, final String text, final String href) {
        final boolean textNN = (text != null);
        final boolean hrefNN = (href != null);
        if (textNN && hrefNN) {
            final Element span = ElementU.addElement(th, Html.SPAN, null,
                    NameTypeValuesU.create(Html.CLASS, spanClass));
            addFooterLinkTo(span, text, href);
        } else if (textNN) {
            ElementU.addElement(th, Html.SPAN, text, NameTypeValuesU.create(Html.CLASS, spanClass));
        } else {
            ElementU.addElement(th, Html.SPAN, UTF16.NBSP, NameTypeValuesU.create(Html.CLASS, spanClass));
        }
    }

    private void addFooterLinkTo(final Element span, final String text, final String href) {
        ElementU.addElement(span, Html.A, text, NameTypeValuesU.create(Html.HREF, href));
    }

    private void addFooterPageRowTo(final Element tfoot, final Page page) {
        final String tableID = table.getID();
        final String targetID = context.getTargetID();
        // table context
        final Element tr = ElementU.addElement(tfoot, Html.TR);
        final Element th = ElementU.addElement(tr, Html.TH, null, NameTypeValuesU.create(
                Html.COLSPAN, Integer.toString(table.getMetaData().size()), Html.CLASS, "status"));
        // first page UI widget
        final SubmitToken tokenFirst = new SubmitToken(App.Target.VIEW_STATE, ViewState.Nav.FIRST, tableID);
        HtmlU.addButton(th, UTF16.ARROW_FIRST, targetID, tokenFirst.toString(), ViewState.Toggle.RIBBON, null);
        // previous page UI widget
        final SubmitToken tokenPrev = new SubmitToken(App.Target.VIEW_STATE, ViewState.Nav.PREVIOUS, tableID);
        HtmlU.addButton(th, UTF16.ARROW_LEFT, targetID, tokenPrev.toString(), ViewState.Toggle.RIBBON, null);
        // text
        final String text = String.format("%d - %d of %d",
                page.getFirstUI(), page.getLastUI(table.getRows()), table.getRows());
        ElementU.addElement(th, Html.SPAN, text);
        // next page UI widget
        final SubmitToken tokenNext = new SubmitToken(App.Target.VIEW_STATE, ViewState.Nav.NEXT, tableID);
        HtmlU.addButton(th, UTF16.ARROW_RIGHT, targetID, tokenNext.toString(), ViewState.Toggle.RIBBON, null);
        // last page UI widget
        final SubmitToken tokenLast = new SubmitToken(App.Target.VIEW_STATE, ViewState.Nav.LAST, tableID);
        HtmlU.addButton(th, UTF16.ARROW_LAST, targetID, tokenLast.toString(), ViewState.Toggle.RIBBON, null);
    }
}