package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.data.DataTypeU;
import io.github.greyp9.arwo.core.xsd.data.NodeType;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactoryU;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceX;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.util.Collection;

@SuppressWarnings("PMD.TooManyMethods")
public class OpUpdate {
    private final XsdTypes xsdTypes;

    public OpUpdate(final XsdTypes xsdTypes) {
        this.xsdTypes = xsdTypes;
    }

    public final Element apply(final Element element, final ValueInstance valueInstance) {
        final TypeInstance typeInstance = valueInstance.getTypeInstance();
        final NameTypeValues nameTypeValues = valueInstance.getNameTypeValues();
        final Collection<TypeInstance> typeInstances = new TypeInstanceX(typeInstance).getPageInstances();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            apply(element, typeInstance, typeInstanceIt, nameTypeValues);
        }
        return element;
    }

    private void apply(final Element update, final TypeInstance parentInstance,
                       final TypeInstance typeInstance, final NameTypeValues nameTypeValues) {
        for (final NameTypeValue nameTypeValue : nameTypeValues) {
            apply(update, parentInstance, typeInstance, nameTypeValue);
        }
    }

    private void apply(final Element update, final TypeInstance parentInstance,
                       final TypeInstance typeInstance, final NameTypeValue nameTypeValue) {
        final String idInstance = typeInstance.getID(parentInstance);
        if (idInstance.equals(nameTypeValue.getName())) {
            final NameTypeValue nameTypeValueIt = new NameTypeValue(typeInstance.getName(), nameTypeValue.getValue());
            applyValue(update, typeInstance, nameTypeValueIt);
        }
    }

    private void applyValue(final Element update, final TypeInstance typeInstance, final NameTypeValue nameTypeValue) {
        final NodeType nodeType = typeInstance.getNodeType();
        if (NodeType.baseType.equals(nodeType)) {
            applyBaseType(update, nameTypeValue, typeInstance);
        } else if (NodeType.attribute.equals(nodeType)) {
            applyAttribute(update, nameTypeValue);
        } else if (NodeType.element.equals(nodeType)) {
            applyElement(update, nameTypeValue);
        } else if (NodeType.choice.equals(nodeType) && (typeInstance instanceof ChoiceTypeInstance)) {
            applyChoice(update, nameTypeValue, (ChoiceTypeInstance) typeInstance);
        }
    }

    private void applyBaseType(
            final Element create, final NameTypeValue nameTypeValue, final TypeInstance typeInstance) {
        final DataType baseTypeRoot = DataTypeU.getRootBaseType(typeInstance.getDataType());
        if (XsdU.NS_URI_XSD.equals(baseTypeRoot.getQName().getNamespaceURI())) {
            ElementU.setTextContentNullable(create, nameTypeValue.getValue());
        }
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

    private void applyChoice(
            final Element update, final NameTypeValue nameTypeValue, final ChoiceTypeInstance choiceInstance) {
        final String value = nameTypeValue.getValueS();
        final TypeInstance typeInstance = choiceInstance.getInstance(value);
        // find the correct place to insert the new content
        getClass();
        // find the current xsd:choice content element (if any)
        final Element currentChoice = findChoice(update, choiceInstance);
        if (currentChoice == null) {
            // insert new
            insertChoice(update, typeInstance, null);
        } else if (!currentChoice.getTagName().equals(value)) {
            // replace existing with new
            insertChoice(update, typeInstance, currentChoice);
            ElementU.detach(currentChoice);
        }
    }

    private Element findChoice(final Element update, final ChoiceTypeInstance choiceInstance) {
        Element currentChoice = null;
        final Collection<TypeInstance> typeInstances = choiceInstance.getTypeInstances().getTypeInstances();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            final Collection<Element> children = ElementU.getChildren(update, typeInstanceIt.getName());
            if (!children.isEmpty()) {
                currentChoice = children.iterator().next();
            }
            if (currentChoice != null) {
                break;
            }
        }
        return currentChoice;
    }

    private void insertChoice(
            final Element update, final TypeInstance typeInstance, final Element insertBefore) {
        final Document document = DocumentFactoryU.generateDocument(xsdTypes, typeInstance, true);
        if (insertBefore == null) {
            ElementU.importNode(document.getDocumentElement(), update);
        } else {
            ElementU.insertBefore(document.getDocumentElement(), insertBefore);
        }
    }
}