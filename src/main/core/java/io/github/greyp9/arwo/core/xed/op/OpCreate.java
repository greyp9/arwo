package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.data.NodeType;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Element;

public class OpCreate {
    private final Element element;

    public OpCreate(final Element element) {
        this.element = element;
    }

    public final Element apply(final ValueInstance valueInstance) {
        final TypeInstance typeInstance = valueInstance.getTypeInstance();
        final Element create = ElementU.addElementBeforeNS(
                element, typeInstance.getName(), typeInstance.getURI(), null);
        final NameTypeValues nameTypeValues = valueInstance.getNameTypeValues();
        for (final TypeInstance typeInstanceIt : typeInstance.getInstances()) {
            for (final NameTypeValue nameTypeValue : nameTypeValues) {
                if (typeInstanceIt.getName().equals(nameTypeValue.getName())) {
                    apply(create, typeInstanceIt, nameTypeValue);
                }
            }
        }
        return create;
    }

    private void apply(final Element create, final TypeInstance typeInstance, final NameTypeValue nameTypeValue) {
        final NodeType nodeType = typeInstance.getNodeType();
        if (NodeType.baseType.equals(nodeType)) {
            applyBaseType();
        } else if (NodeType.attribute.equals(nodeType)) {
            applyAttribute(create, nameTypeValue);
        } else if (NodeType.element.equals(nodeType)) {
            applyElement(create, nameTypeValue);
        } else if (NodeType.choice.equals(nodeType) && (typeInstance instanceof ChoiceTypeInstance)) {
            applyChoice();
        }

    }

    private void applyBaseType() {
        getClass();
    }

    private void applyAttribute(final Element create, final NameTypeValue nameTypeValue) {
        ElementU.setAttribute(create, nameTypeValue.getName(), nameTypeValue.getValue());
    }

    private void applyElement(final Element create, final NameTypeValue nameTypeValue) {
        ElementU.addElement(create, nameTypeValue.getName(), nameTypeValue.getValue());
    }

    private void applyChoice() {
        getClass();
    }
}
