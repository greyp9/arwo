package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

public class OpDelete {

    public final Element apply(final Element element) {
        if (element.getParentNode() != null) {
            ElementU.detach(element);
        }
        return element;
    }
}
