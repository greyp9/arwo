package io.github.greyp9.arwo.core.xed.view.html.type;

import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceDrillDown;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

public class DrillDownHtmlView {
    private final ViewInstanceDrillDown viewInstance;

    public DrillDownHtmlView(final ViewInstanceDrillDown viewInstance) {
        this.viewInstance = viewInstance;
    }

    public final void addContentTo(final Element td) {
        // link to content
        final XedCursor cursor = viewInstance.getCursor();
        final XedCursor cursorChild = viewInstance.getCursorDrillDown();
        final String href = getURI(cursor, cursorChild);
        // UI label for content
        final int typeCount = cursor.getTypeCount(cursorChild.getTypeInstance());
        getAnchor(td, href, String.format("%s (%d)", UTF16.ARROW_RIGHT, typeCount));
    }

    private String getURI(final XedCursor cursor, final XedCursor cursorChild) {
        final TypeInstance typeInstance = cursorChild.getTypeInstance();
        String href = cursorChild.getURI();
        // if this is a [1..1] TypeInstance, provide link straight to object
        if (typeInstance.isSingleton()) {
            final Element singleton = ElementU.getChild(cursor.getElement(), typeInstance.getQName());
            if (singleton != null) {
                final XedCursor child = new XedNav(cursor.getXed()).find(singleton, viewInstance.getCursor());
                href = child.getURI();
            }
        }
        return href;
    }

    private Element getAnchor(final Element html, final String href, final String label) {
        final String uri = viewInstance.getBaseURI() + href;
        final NameTypeValues attrs = NameTypeValuesU.create(Html.CLASS, "menu", Html.HREF, uri);
        return ElementU.addElement(html, Html.A, label, attrs);
    }
}
