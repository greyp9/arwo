package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;

import java.util.Collection;

public class OpPrune {
    private final XedNav nav;

    public OpPrune(final Xed xed) {
        this.nav = new XedNav(xed);
    }

    public final void apply(final XedCursor cursor) {
        // elements (head recursion)
        final Element element = cursor.getElement();
        final Collection<Element> children = ElementU.getChildren(element);
        for (final Element child : children) {
            final XedCursor cursorIt = nav.find(child, cursor);
            if (cursorIt == null) {
                ElementU.detach(child);
            } else {
                apply(cursorIt);
            }
        }
        // attributes
        final Collection<Attr> attrs = ElementU.getAttributes(element);
        for (final Attr attr : attrs) {
            final boolean isNamespace = attr.getName().equals(XsdU.NS_PREFIX_XMLNS);
            final boolean isInModel = (nav.find(attr, cursor) != null);
            if ((!isNamespace) && (!isInModel)) {
                ElementU.detach(attr);
            }
        }
    }
}
