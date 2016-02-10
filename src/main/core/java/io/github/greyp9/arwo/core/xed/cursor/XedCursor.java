package io.github.greyp9.arwo.core.xed.cursor;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.data.NodeType;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.util.Collection;

public class XedCursor {
    private final Xed xed;
    private final XedCursor parent;
    private final Node node;
    private final Integer ordinal;
    private final TypeInstance typeInstance;

    public final Xed getXed() {
        return xed;
    }

    public final XedCursor getParent() {
        return parent;
    }

    public final Node getNode() {
        return node;
    }

    public final Integer getOrdinal() {
        return ordinal;
    }

    public final TypeInstance getTypeInstance() {
        return typeInstance;
    }

    public XedCursor(final Xed xed, final XedCursor parent, final Node node,
                     final Integer ordinal, final TypeInstance typeInstance) {
        this.xed = xed;
        this.parent = parent;
        this.node = node;
        this.ordinal = ordinal;
        this.typeInstance = typeInstance;
    }

    public final XedCursor getConcrete() {
        XedCursor cursor = this;
        while (cursor.getNode() == null) {
            cursor = cursor.getParent();
        }
        return cursor;
    }

    public final XedCursor getParentConcrete() {
        XedCursor parentConcrete = parent;
        while ((parentConcrete != null) && (parentConcrete.getNode() == null)) {
            parentConcrete = parentConcrete.getParent();
        }
        return parentConcrete;
    }

    public final Element getElement() {
        return ((node instanceof Element) ? ((Element) node) : null);
    }

    public final TypeInstance getChildInstance(final String name) {
        return typeInstance.getInstance(name);
    }

    public final Element getChild(final TypeInstance childInstance) {
        return ElementU.getChild(getElement(), childInstance.getQName());
    }

    public final Collection<Element> getChildren(final TypeInstance childInstance) {
        return ElementU.getChildren(getElement(), childInstance.getName());
    }

    public final int getTypeCount(final TypeInstance childInstance) {
        int value = 0;
        final Element element = getElement();
        final TypeInstance.NodeType nodeType = (childInstance == null) ? null : childInstance.getNodeType();
        if (element == null) {
            value = 0;
        } else if (TypeInstance.NodeType.attribute.equals(nodeType)) {
            final Attr attr = ElementU.getAttributeNode(element, childInstance.getName());
            value = (attr == null) ? 0 : 1;
        } else if (TypeInstance.NodeType.element.equals(nodeType)) {
            final Collection<Element> children = ElementU.getChildren(element, childInstance.getName());
            value = children.size();
        } else if (TypeInstance.NodeType.baseType.equals(nodeType)) {
            value = 1;
        } else if (TypeInstance.NodeType.choice.equals(nodeType) && (childInstance instanceof ChoiceTypeInstance)) {
            value = 0;
        }
        return value;
    }

    public final String getValue(final TypeInstance childInstance) {
        String value = null;
        final Element element = getElement();
        final TypeInstance.NodeType nodeType = (childInstance == null) ? null : childInstance.getNodeType();
        if (TypeInstance.NodeType.attribute.equals(nodeType)) {
            value = ElementU.getAttribute(element, childInstance.getName());
        } else if (TypeInstance.NodeType.element.equals(nodeType)) {
            value = ElementU.getTextContent(ElementU.getChild(element, childInstance.getQName()));
        } else if (TypeInstance.NodeType.baseType.equals(nodeType)) {
            value = ElementU.getTextContent(element);
        } else if (TypeInstance.NodeType.choice.equals(nodeType) && (childInstance instanceof ChoiceTypeInstance)) {
            value = getValue(element, (ChoiceTypeInstance) childInstance);
        }
        return value;
    }

    private String getValue(final Element element, final ChoiceTypeInstance choiceInstance) {
        String value = null;
        final Collection<TypeInstance> typeInstances = choiceInstance.getTypeInstances().getTypeInstances();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            final Element child = ((element == null) ? null : ElementU.getChild(element, typeInstanceIt.getQName()));
            if (child != null) {
                value = typeInstanceIt.getName();
                break;
            }
        }
        return value;
    }

    public static String getID(final TypeInstance typeInstance, final Integer ordinal) {
        final byte[] id = UTF8Codec.toBytes(getIdentity(typeInstance, ordinal));
        return CRCU.crc32String(id).substring(Const.STRIP_LEADING);
    }

    private static String getIdentity(final TypeInstance typeInstance, final Integer ordinal) {
        return String.format("%s/%d", typeInstance.toString(), ordinal);
    }

    public final String getURI() {
        return ((parent == null) ? Http.Token.SLASH :
                (parent.getURI() + getID(typeInstance, ordinal) + Http.Token.SLASH));
    }

    private static class Const {
        private static final int STRIP_LEADING = 3;
    }
}
