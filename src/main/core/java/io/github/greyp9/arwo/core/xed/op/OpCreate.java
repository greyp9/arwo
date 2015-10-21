package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.data.NodeType;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactoryU;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class OpCreate {
    private final Element element;
    private final XsdTypes xsdTypes;

    public OpCreate(final Element element, final XsdTypes xsdTypes) {
        this.element = element;
        this.xsdTypes = xsdTypes;
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
            applyChoice(create, nameTypeValue, (ChoiceTypeInstance) typeInstance);
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

    private void applyChoice(
            final Element create, final NameTypeValue nameTypeValue, final ChoiceTypeInstance choiceInstance) {
        final String value = nameTypeValue.getValueS();
        final TypeInstance typeInstance = choiceInstance.getInstance(value);
        final Document document = DocumentFactoryU.generateDocument(xsdTypes, typeInstance, true);
        ElementU.importNode(document.getDocumentElement(), create);
    }
}
