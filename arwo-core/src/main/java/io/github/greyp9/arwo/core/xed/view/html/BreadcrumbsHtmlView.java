package io.github.greyp9.arwo.core.xed.view.html;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class BreadcrumbsHtmlView {
    private final String baseURI;
    private final XedCursor cursor;
    private final Bundle bundle;

    public BreadcrumbsHtmlView(final String baseURI, final XedCursor cursor, final Bundle bundle) {
        this.baseURI = baseURI;
        this.cursor = cursor;
        this.bundle = bundle;
    }

    public final String addContentTo(final Element html) {
        String context = null;
        final Element divMenus = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENUS));
        final Element divMenu = ElementU.addElement(divMenus, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENU));
        ElementU.addElement(divMenu, Html.SPAN, UTF16.DOCUMENT_BRACKETS, NTV.create(Html.CLASS, App.CSS.MENU));
        final Collection<XedCursor> cursorBreadcrumbs = getBreadcrumbs();
        final XsdBundle xsdBundle = cursor.getXed().getXsdBundle();
        for (final XedCursor cursorCrumb : cursorBreadcrumbs) {
            final XedCursor cursorParent = cursorCrumb.getParentConcrete();
            final TypeInstance instanceParent = ((cursorParent == null) ? null : cursorParent.getTypeInstance());
            final String label = xsdBundle.getLabel(instanceParent, cursorCrumb.getTypeInstance());
            context = label;
            final String resource = baseURI + cursorCrumb.getURI();
            if (cursorParent != null) {
                ElementU.addElement(divMenu, Html.SPAN, UTF16.LIST_EXPAND, NTV.create(Html.CLASS, App.CSS.MENU));
            }
            addAnchorBreadcrumb(cursorCrumb, divMenu, bundle.getString(Const.TITLE_BREADCRUMB), label, resource);
        }
        return context;
    }

    private void addAnchorBreadcrumb(final XedCursor cursorCrumb, final Element parent,
                                     final String title, final String label, final String resource) {
        final NameTypeValues attrs = NTV.create(Html.CLASS, App.CSS.MENU, Html.TITLE, title, Html.HREF, resource);
        final Element anchor = ElementU.addElement(parent, Html.A, label, attrs);
        // highlight cursor position
        if (cursorCrumb.equals(cursor)) {
            ElementU.setAttribute(anchor, Html.CLASS, Value.join(Html.SPACE, App.CSS.MENU, App.CSS.ACTIVE));
        }
    }

    private Collection<XedCursor> getBreadcrumbs() {
        final List<XedCursor> cursorCrumbs = new ArrayList<XedCursor>();
        XedCursor cursorCrumb = cursor;
        while (cursorCrumb != null) {
            cursorCrumbs.add(0, cursorCrumb);
            cursorCrumb = cursorCrumb.getParentConcrete();
        }
        return cursorCrumbs;
    }

    private static class Const {
        private static final String TITLE_BREADCRUMB = "menu.document.navigate.DETAIL";
    }
}
