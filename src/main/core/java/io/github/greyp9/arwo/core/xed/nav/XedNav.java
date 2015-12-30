package io.github.greyp9.arwo.core.xed.nav;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import javax.xml.namespace.QName;
import java.io.IOException;

public class XedNav {
    private final Xed xed;

    public final Xed getXed() {
        return xed;
    }

    public XedNav(final Xed xed) {
        this.xed = xed;
    }

    public final XedCursor getRoot() {
        final Element element = xed.getDocument().getDocumentElement();
        final QName qname = QNameU.getQName(element);
        final TypeInstance typeInstance = xed.getXsdTypes().getElementType(qname.toString());
        return new XedCursor(xed, null, element, 0, typeInstance);
    }

    public final XedCursor find(final String path) {
        return new XedNavPath().find(path, getRoot());
    }

    public final XedCursor find(final Node node) {
        return ((node == null) ? null : new XedNavNode().find(node, getRoot()));
    }

    public final XedCursor find(final Node node, final XedCursor cursor) {
        return ((node == null) ? null : new XedNavNode().findChild(node, cursor));
    }

    public final XedCursor find(final TypeInstance typeInstance, final XedCursor cursor) {
        return new XedNavNode().findTypeInstance(typeInstance, cursor);
    }

    public final XedCursor find(final String typeInstanceName, final XedCursor cursor) {
        return ((cursor == null) ? null : new XedNavNode().findTypeInstance(typeInstanceName, cursor));
    }

    public final XedCursor findChild(final TypeInstance typeInstance, final XedCursor cursor) {
        return ((cursor == null) ? null : new XedNavNode().findTypeInstanceChild(typeInstance.getName(), cursor));
    }

    public final XedCursor findChild(final String typeInstanceName, final XedCursor cursor) {
        return ((cursor == null) ? null : new XedNavNode().findTypeInstanceChild(typeInstanceName, cursor));
    }

    public final XedCursor findX(final String xpath) throws IOException {
        return find(xed.getXPather().getElement(xpath));
    }
}
