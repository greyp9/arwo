package io.github.greyp9.arwo.core.xed.cursor;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.data.NodeType;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
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

    public final XedCursor getParentConcrete() {
        XedCursor parentConcrete = parent;
        while (parentConcrete.getNode() == null) {
            parentConcrete = parentConcrete.getParent();
        }
        return parentConcrete;
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

    public final Element getElement() {
        return ((node instanceof Element) ? ((Element) node) : null);
    }

    public final String getValue(final TypeInstance childInstance) {
        String value = null;
        final Element element = getElement();
        final NodeType nodeType = (childInstance == null) ? null : childInstance.getNodeType();
        if (NodeType.attribute.equals(nodeType)) {
            value = ElementU.getAttribute(element, childInstance.getName());
        } else if (NodeType.element.equals(nodeType)) {
            value = ElementU.getTextContent(ElementU.getChild(element, childInstance.getQName()));
        } else if (NodeType.baseType.equals(nodeType)) {
            value = ElementU.getTextContent(element);
        } else if (NodeType.choice.equals(nodeType) && (childInstance instanceof ChoiceTypeInstance)) {
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
        return ((parent == null) ? "/" : (parent.getURI() + getID(typeInstance, ordinal) + "/"));
    }

    private static class Const {
        private static final int STRIP_LEADING = 3;
    }
}
