package io.github.greyp9.arwo.core.xed.nav;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.data.NodeType;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceTraversal;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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

    public final XedCursor findChild(final Node node, final XedCursor cursor) {
        final XedCursor cursorTI = findTypeInstance(node.getLocalName(), cursor);
        XedCursor cursorChild = null;
        if (node instanceof Attr) {
            cursorChild = findChildAttr((Attr) node, cursor, cursorTI);
        } else if (node instanceof Element) {
            cursorChild = findChildElement((Element) node, cursor, cursorTI);
        }
        return cursorChild;
    }

    public final XedCursor findTypeInstanceChild(final String name, final XedCursor cursor) {
        final Element element = cursor.getElement();
        final XedCursor cursorTI = findTypeInstance(name, cursor);
        XedCursor cursorChild = null;
        final TypeInstance typeInstance = cursorTI.getTypeInstance();
        final NodeType nodeType = (typeInstance == null) ? null : typeInstance.getNodeType();
        if (NodeType.attribute.equals(nodeType)) {
            cursorChild = findChildAttr(ElementU.getAttributeNode(element, name), cursor, cursorTI);
        } else if (NodeType.element.equals(nodeType)) {
            cursorChild = findChildElement(ElementU.getChild(element, typeInstance.getQName()), cursor, cursorTI);
        }
        return cursorChild;
    }

    public final XedCursor findTypeInstance(final TypeInstance typeInstance, final XedCursor cursor) {
        return new XedCursor(cursor.getXed(), cursor, null, null, typeInstance);
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public final XedCursor findTypeInstance(final String name, final XedCursor cursor) {
        XedCursor cursorTI;
        final TypeInstance typeInstance = cursor.getTypeInstance();
        final TypeInstance typeInstanceChild = typeInstance.getInstance(name);
        if (typeInstance.getInstances().contains(typeInstanceChild)) {
            cursorTI = new XedCursor(cursor.getXed(), cursor, null, null, typeInstanceChild);
        } else {
            cursorTI = cursor;
            final TypeInstanceTraversal traversal = new TypeInstanceTraversal(typeInstance);
            final List<TypeInstance> typeInstances = traversal.getForName(typeInstanceChild.getName());
            for (final TypeInstance typeInstanceIt : typeInstances) {
                cursorTI = new XedCursor(cursor.getXed(), cursorTI, null, null, typeInstanceIt);
            }
        }
        return cursorTI;
    }

    private XedCursor findChildAttr(final Attr attr, final XedCursor cursorParent, final XedCursor cursorTI) {
        final Xed xed = cursorParent.getXed();
        final Element elementParent = cursorParent.getElement();
        final TypeInstance typeInstance = cursorTI.getTypeInstance();
        final Attr attrIt = (elementParent == null) ? null : ElementU.getAttributeNode(elementParent, attr.getName());
        return ((attrIt == null) ? null : new XedCursor(xed, cursorTI, attrIt, 0, typeInstance));
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private XedCursor findChildElement(final Element child, final XedCursor cursorParent, final XedCursor cursorTI) {
        final Xed xed = cursorParent.getXed();
        final Element elementParent = cursorParent.getElement();
        final TypeInstance typeInstance = cursorTI.getTypeInstance();
        XedCursor cursor = new XedCursor(xed, cursorTI, null, null, typeInstance);
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
