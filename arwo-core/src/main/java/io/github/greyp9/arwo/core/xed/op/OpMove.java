package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

public class OpMove {
    private final XsdTypes xsdTypes;

    public OpMove(final XsdTypes xsdTypes) {
        this.xsdTypes = xsdTypes;
    }

    public final Element moveUp(final Element element) {
        xsdTypes.getClass();

        final Element previous = ElementU.getPreviousElement(element);
        if (previous != null) {
            final Element parent = (Element) element.getParentNode();
            final Node nextSibling = element.getNextSibling();
            parent.insertBefore(element, previous);
            parent.insertBefore(previous, nextSibling);
        }
        return element;
    }

    public final Element moveDown(final Element element) {
        xsdTypes.getClass();

        final Element next = ElementU.getNextElement(element);
        if (next != null) {
            final Element parent = (Element) element.getParentNode();
            final Node nextSibling = next.getNextSibling();
            parent.insertBefore(next, element);
            parent.insertBefore(element, nextSibling);
        }
        return element;
    }
}
