package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.data.NodeType;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Element;

import java.util.Collection;

public class OpUpdate {
    private final Element element;

    public OpUpdate(final Element element) {
        this.element = element;
    }

    public final Element apply(final ValueInstance valueInstance) {
        final TypeInstance typeInstance = valueInstance.getTypeInstance();
        final Element update = element;
        final NameTypeValues nameTypeValues = valueInstance.getNameTypeValues();
        for (final TypeInstance typeInstanceIt : typeInstance.getInstances()) {
            for (final NameTypeValue nameTypeValue : nameTypeValues) {
                if (typeInstanceIt.getName().equals(nameTypeValue.getName())) {
                    apply(update, typeInstanceIt, nameTypeValue);
                }
            }
        }
        return update;
    }

    private void apply(final Element update, final TypeInstance typeInstance, final NameTypeValue nameTypeValue) {
        final NodeType nodeType = typeInstance.getNodeType();
        if (NodeType.baseType.equals(nodeType)) {
            applyBaseType();
        } else if (NodeType.attribute.equals(nodeType)) {
            applyAttribute(update, nameTypeValue);
        } else if (NodeType.element.equals(nodeType)) {
            applyElement(update, nameTypeValue);
        } else if (NodeType.choice.equals(nodeType) && (typeInstance instanceof ChoiceTypeInstance)) {
            applyChoice();
        }

    }

    private void applyBaseType() {
        getClass();
    }

    private void applyAttribute(final Element update, final NameTypeValue nameTypeValue) {
        ElementU.setAttribute(update, nameTypeValue.getName(), nameTypeValue.getValue());
    }

    private void applyElement(final Element update, final NameTypeValue nameTypeValue) {
        final Element childMatch = findElement(update, nameTypeValue);
        ElementU.addElement(update, nameTypeValue.getName(), nameTypeValue.getValue(), childMatch);
        ElementU.detach(childMatch);
    }

    private Element findElement(final Element update, final NameTypeValue nameTypeValue) {
        final Collection<Element> children = ElementU.getChildren(update);
        Element childMatch = null;
        for (final Element child : children) {
            if (child.getTagName().equals(nameTypeValue.getName())) {
                childMatch = child;
            }
        }
        return childMatch;
    }

    private void applyChoice() {
        getClass();
    }
}
