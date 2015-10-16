package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

public class OpDelete {
    private final Element element;

    public OpDelete(final Element element) {
        this.element = element;
    }

    public final Element apply() {
        if (element.getParentNode() != null) {
            ElementU.detach(element);
        }
        return element;
    }
}
