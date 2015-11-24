package io.github.greyp9.arwo.core.xed.view.html.type;

import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstance;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceDrillDown;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceFactory;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

import java.util.Collection;

public class DrillDownHtmlView {
    private final ViewInstanceDrillDown viewInstance;

    public DrillDownHtmlView(final ViewInstanceDrillDown viewInstance) {
        this.viewInstance = viewInstance;
    }

    public final void addContentTo(final Element td) {
        // link to content
        final XedCursor cursor = viewInstance.getCursor();
        final XedCursor cursorChild = viewInstance.getCursorDrillDown();
        final String href = getCursorTypeInstance(cursorChild).getURI();
        // UI label for content
        final XsdBundle bundle = cursor.getXed().getXsdBundle();
        final int typeCount = cursor.getTypeCount(cursorChild.getTypeInstance());
        final String title = bundle.getDetail(cursor.getTypeInstance(), viewInstance.getTypeInstance());
        getAnchor(td, href, String.format("%s (%d)", UTF16.ARROW_RIGHT, typeCount), title);
    }

    private XedCursor getCursorTypeInstance(final XedCursor cursorTI) {
        XedCursor cursor = cursorTI;
        final XedNav nav = new XedNav(cursorTI.getXed());
        // if this is a [1..1] TypeInstance, provide link straight to element
        final TypeInstance typeInstance = cursor.getTypeInstance();
        if (typeInstance.isSingleton()) {
            final XedCursor cursorConcrete = cursor.getParentConcrete();
            final Element singleton = cursorConcrete.getChild(typeInstance);
            if (singleton != null) {
                cursor = getCursorConcrete(nav.find(singleton, cursorConcrete));
            }
        }
        return cursor;
    }

    private XedCursor getCursorConcrete(final XedCursor cursorConcrete) {
        XedCursor cursor = cursorConcrete;
        // if this location is 1 drill down node, provide link straight to type instance
        final Collection<ViewInstance> pageInstances = new ViewInstanceFactory("", cursor).getPageInstances();
        final boolean isSingleton = (pageInstances.size() == 1);
        if (isSingleton) {
            final ViewInstance viewInstanceIt = pageInstances.iterator().next();
            if (viewInstanceIt instanceof ViewInstanceDrillDown) {
                cursor = getCursorTypeInstance(((ViewInstanceDrillDown) viewInstanceIt).getCursorDrillDown());
            }
        }
        return cursor;
    }

    private Element getAnchor(final Element html, final String href, final String label, final String title) {
        final String uri = viewInstance.getBaseURI() + href;
        final NameTypeValues attrs = NameTypeValuesU.create(Html.CLASS, "menu", Html.HREF, uri, Html.TITLE, title);
        return ElementU.addElement(html, Html.A, label, attrs);
    }
}
