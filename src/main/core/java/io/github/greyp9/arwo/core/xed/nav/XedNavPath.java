package io.github.greyp9.arwo.core.xed.nav;

import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.data.NodeType;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;

import java.util.Collection;

public class XedNavPath {

    public final XedCursor find(final String path, final XedCursor cursor) {
        XedCursor cursorFind;
        final Pather pather = new Pather(path);
        final String leftToken = pather.getLeftToken();
        final String right = pather.getRight();
        if (leftToken == null) {
            cursorFind = null;
        } else if ((leftToken.length() == 0) && (right == null)) {
            cursorFind = cursor;
        } else {
            cursorFind = find(right, cursor, leftToken);
        }
        return cursorFind;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private XedCursor find(final String path, final XedCursor cursor, final String token) {
        XedCursor cursorFind = null;
        final Collection<TypeInstance> typeInstances = cursor.getTypeInstance().getInstances();
        for (final TypeInstance typeInstance : typeInstances) {
            final String id = XedCursor.getID(typeInstance, null);
            if (id.equals(token)) {
                final XedCursor cursorType = new XedCursor(cursor.getXed(), cursor, null, null, typeInstance);
                cursorFind = find(path, cursor, cursorType);
                break;
            }
        }
        return cursorFind;
    }

    private XedCursor find(final String path, final XedCursor cursor, final XedCursor cursorType) {
        XedCursor cursorFind;
        final Pather pather = new Pather(path);
        final String leftToken = pather.getLeftToken();
        final String right = pather.getRight();
        if (leftToken == null) {
            cursorFind = null;
        } else if ((leftToken.length() == 0) && (right == null)) {
            cursorFind = cursorType;
        } else {
            cursorFind = find(right, cursor, cursorType, leftToken);
        }
        return cursorFind;
    }

    private XedCursor find(final String path, final XedCursor cursor, final XedCursor cursorType, final String token) {
        XedCursor cursorFind = null;
        final TypeInstance typeInstance = cursorType.getTypeInstance();
        final TypeInstance.NodeType nodeType = typeInstance.getNodeType();
        if (TypeInstance.NodeType.attribute.equals(nodeType)) {
            cursorFind = findAttr(cursor, cursorType);
        } else if (TypeInstance.NodeType.element.equals(nodeType)) {
            cursorFind = findElement(path, cursor, cursorType, token);
        } else if (TypeInstance.NodeType.choice.equals(nodeType)) {
            cursorFind = findChoice(path, cursor, cursorType, token);
        }
        return cursorFind;
    }

    private XedCursor findAttr(final XedCursor cursor, final XedCursor cursorType) {
        final TypeInstance typeInstance = cursorType.getTypeInstance();
        final Attr attrIt = ElementU.getAttributeNode(cursor.getElement(), typeInstance.getName());
        return ((attrIt == null) ? null : new XedCursor(cursor.getXed(), cursorType, attrIt, 0, typeInstance));
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private XedCursor findElement(
            final String path, final XedCursor cursor, final XedCursor cursorType, final String token) {
        XedCursor cursorFind = null;
        final TypeInstance typeInstance = cursorType.getTypeInstance();
        final Collection<Element> children = ElementU.getChildren(cursor.getElement(), typeInstance.getName());
        int ordinal = -1;
        for (final Element child : children) {
            final String id = XedCursor.getID(typeInstance, ++ordinal);
            if (id.equals(token)) {
                final XedCursor cursorChild = new XedCursor(cursor.getXed(), cursorType, child, ordinal, typeInstance);
                cursorFind = find(path, cursorChild);
            }
        }
        return cursorFind;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private XedCursor findChoice(
            final String path, final XedCursor cursor, final XedCursor cursorType, final String token) {
        XedCursor cursorFind = null;
        final ChoiceTypeInstance choiceInstance = (ChoiceTypeInstance) cursorType.getTypeInstance();
        final Collection<TypeInstance> typeInstances = choiceInstance.getTypeInstances().getTypeInstances();
        for (final TypeInstance typeInstance : typeInstances) {
            final String id = XedCursor.getID(typeInstance, null);
            if (id.equals(token)) {
                final XedCursor cursorChoice = new XedCursor(cursor.getXed(), cursorType, null, null, typeInstance);
                cursorFind = find(path, cursor, cursorChoice);
                break;
            }
        }
        return cursorFind;
    }
}
