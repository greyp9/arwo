package io.github.greyp9.arwo.core.table.html;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.HtmlU;
import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.value.NTV;
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

    public final void addContentTo(final Element tableHtml, final boolean openTB) {
        if (openTB) {
            final Element tfoot = ElementU.addElement(tableHtml, Html.TFOOT, null, NTV.create(Html.CLASS, Html.TABLE));
            addFooterRowTo(tfoot);
            if (context.getViewState().getPage() != null) {
                addFooterPageRowTo(tfoot, context.getViewState().getPage());
            }
            ElementU.detachOnEmpty(tfoot);
        }
    }

    private void addFooterRowTo(final Element tfoot) {
        final Properties properties = table.getProperties();
        final String footerL = properties.getProperty(Table.Const.FOOTER_L);
        final String footerC = properties.getProperty(Table.Const.FOOTER_C);
        final String footerR = properties.getProperty(Table.Const.FOOTER_R);
        if ((footerC != null) || (footerL != null) || (footerR != null)) {
            final Element tr = ElementU.addElement(tfoot, Html.TR);
            final Element th = ElementU.addElement(tr, Html.TH, null, NTV.create(
                    Html.COLSPAN, Integer.toString(table.getMetaData().size()), Html.CLASS, App.CSS.STATUS));
            addFooterTo(th, App.CSS.LEFT, footerL, properties.getProperty(Table.Const.FOOTER_HREF_L));
            addFooterTo(th, App.CSS.CENTER, footerC, properties.getProperty(Table.Const.FOOTER_HREF_C));
            addFooterTo(th, App.CSS.RIGHT, footerR, properties.getProperty(Table.Const.FOOTER_HREF_R));
        }
    }

    private void addFooterTo(final Element th, final String spanClass, final String text, final String href) {
        final boolean textNN = (text != null);
        final boolean hrefNN = (href != null);
        if (textNN && hrefNN) {
            final Element span = ElementU.addElement(th, Html.SPAN, null, NTV.create(Html.CLASS, spanClass));
            addFooterLinkTo(span, text, href);
        } else if (textNN) {
            ElementU.addElement(th, Html.SPAN, text, NTV.create(Html.CLASS, spanClass));
        } else {
            ElementU.addElement(th, Html.SPAN, UTF16.NBSP, NTV.create(Html.CLASS, spanClass));
        }
    }

    private void addFooterLinkTo(final Element span, final String text, final String href) {
        ElementU.addElement(span, Html.A, text, NTV.create(Html.HREF, href, Html.ACCESSKEY, Html.VALUE_3));
    }

    private void addFooterPageRowTo(final Element tfoot, final Page page) {
        final Bundle bundle = context.getBundle();
        // table context
        final Element tr = ElementU.addElement(tfoot, Html.TR);
        final Element th = ElementU.addElement(tr, Html.TH, null, NTV.create(
                Html.COLSPAN, Integer.toString(table.getMetaData().size()), Html.CLASS, App.CSS.STATUS));
        // navigation
        addFooterControl(th, ViewState.Nav.FIRST, UTF16.ARROW_FIRST, bundle.getString("table.page.first"));
        addFooterControl(th, ViewState.Nav.PREVIOUS, UTF16.ARROW_LEFT, bundle.getString("table.page.prev"));
        // text
        final String text = bundle.format("table.page.n.to.m.of.x",
                page.getFirstUI(), page.getLastUI(table.getRows()), table.getRows());
        ElementU.addElement(th, Html.SPAN, text);
        // navigation
        addFooterControl(th, ViewState.Nav.NEXT, UTF16.ARROW_RIGHT, bundle.getString("table.page.next"));
        addFooterControl(th, ViewState.Nav.LAST, UTF16.ARROW_LAST, bundle.getString("table.page.last"));
    }

    private void addFooterControl(final Element th, final String action, final String label, final String title) {
        final SubmitToken token = new SubmitToken(App.Target.VIEW_STATE, action, table.getID());
        HtmlU.addButton(th, label, context.getSubmitID(), token.toString(), ViewState.Toggle.RIBBON, title);
    }
}
