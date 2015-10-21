package io.github.greyp9.arwo.core.xed.nav;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.util.ArrayList;
import java.util.Collection;

public class XedNavNode {

    public final XedCursor find(final Node node, final XedCursor cursor) {
        final Element documentElement = node.getOwnerDocument().getDocumentElement();
        Node nodeIt = node;
        final ArrayList<Node> traversal = new ArrayList<Node>();
        while (nodeIt != null) {
            if ((nodeIt instanceof Element) && (!nodeIt.equals(documentElement))) {
                traversal.add(0, nodeIt);
            }
            nodeIt = nodeIt.getParentNode();
        }
        XedCursor cursorIt = cursor;
        while ((!traversal.isEmpty()) && (cursorIt != null)) {
            cursorIt = findChild(traversal.remove(0), cursorIt);
        }
        return cursorIt;
    }

    private XedCursor findChild(final Node node, final XedCursor cursor) {
        final XedCursor cursorTI = findTypeInstance(node.getLocalName(), cursor);
        XedCursor cursorChild = null;
        if (node instanceof Attr) {
            cursorChild = findChildAttr((Attr) node, cursor, cursorTI);
        } else if (node instanceof Element) {
            cursorChild = findChildElement((Element) node, cursor, cursorTI);
        }
        return cursorChild;
    }

    public final XedCursor findTypeInstance(final String name, final XedCursor cursor) {
        final TypeInstance typeInstance = cursor.getTypeInstance();
        final TypeInstance typeInstanceChild = typeInstance.getInstance(name);
        return new XedCursor(cursor.getXed(), cursor, null, null, typeInstanceChild);
    }

    private XedCursor findChildAttr(final Attr attr, final XedCursor cursorParent, final XedCursor cursorTI) {
        final Xed xed = cursorParent.getXed();
        final Element elementParent = cursorParent.getElement();
        final TypeInstance typeInstance = cursorTI.getTypeInstance();
        final Attr attrIt = ElementU.getAttributeNode(elementParent, attr.getName());
        return ((attrIt == null) ? null : new XedCursor(xed, cursorTI, attrIt, 0, typeInstance));
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private XedCursor findChildElement(final Element child, final XedCursor cursorParent, final XedCursor cursorTI) {
        final Xed xed = cursorParent.getXed();
        final Element elementParent = cursorParent.getElement();
        final TypeInstance typeInstance = cursorTI.getTypeInstance();
        XedCursor cursor = null;
        final Collection<Element> children = ElementU.getChildren(elementParent);
        int ordinal = -1;
        for (final Element childIt : children) {
            ++ordinal;
            if (childIt.equals(child)) {
                cursor = new XedCursor(xed, cursorTI, childIt, ordinal, typeInstance);
                break;
            }
        }
        return cursor;
    }
}
