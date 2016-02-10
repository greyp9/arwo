package io.github.greyp9.arwo.core.xsd.document;

import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.data.NodeType;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.util.ArrayList;
import java.util.Collection;

public class DocumentFactory {
    private final TypeDefinitions typeDefinitions;
    private final TypeInstanceFactory instanceFactory;
    private final boolean includeOptional;

    public final TypeInstanceFactory getInstanceFactory() {
        return instanceFactory;
    }

    public DocumentFactory(final TypeDefinitions typeDefinitions) {
        this(typeDefinitions, false);
    }

    public DocumentFactory(final TypeDefinitions typeDefinitions, final boolean includeOptional) {
        this.typeDefinitions = typeDefinitions;
        this.instanceFactory = new TypeInstanceFactory();
        this.includeOptional = includeOptional;
    }

    public final Document generateEmpty(final QName name) {
        final TypeInstance typeInstance = typeDefinitions.getElementTypes().get(name.toString());
        return generateEmpty(typeInstance.getQName(), typeInstance);
    }

    @SuppressWarnings("unused")
    public final Document generateEmpty(final TypeInstance typeInstance) {
        return generateEmpty(typeInstance.getQName(), typeInstance);
    }

    public final Document generateEmpty(final QName qname, final TypeInstance typeInstance) {
        final Document document = DocumentU.createDocumentSafe(qname);
        if (document != null) {
            addContent(document.getDocumentElement(), typeInstance);
        }
        return document;
    }

    private void addContent(final Element element, final TypeInstance typeInstance) {
        ElementU.setTextContent(element, instanceFactory.getDefaultValue(typeInstance));
        final DataType dataType = typeInstance.getDataType();
        for (final TypeInstance typeInstanceIt : dataType.getInstances()) {
            final TypeInstance.NodeType nodeType = typeInstanceIt.getNodeType();
            if (TypeInstance.NodeType.attribute.equals(nodeType)) {
                addAttribute(element, typeInstanceIt);
            } else if (TypeInstance.NodeType.element.equals(nodeType)) {
                addElement(element, typeInstanceIt);
            } else if (TypeInstance.NodeType.choice.equals(nodeType) &&
                    (typeInstanceIt instanceof ChoiceTypeInstance)) {
                addChoice(element, (ChoiceTypeInstance) typeInstanceIt);
            } else {
                throw new IllegalStateException(dataType.getQName().toString());
            }
        }
    }

    private void addAttribute(final Element element, final TypeInstance typeInstance) {
        final boolean required = XsdU.REQUIRED.equals(typeInstance.getUse());
        if ((required) || (includeOptional)) {
            final String name = typeInstance.getName();
            ElementU.setAttribute(element, name, instanceFactory.getDefaultValue(typeInstance));
        }
    }

    private void addElement(final Element element, final TypeInstance typeInstance) {
        final QName name = typeInstance.getQName();
        int minOccurs = NumberU.toInt(typeInstance.getMinOccurs(), 1);
        if (includeOptional) {
            minOccurs = Math.max(minOccurs, 1);
        }
        for (int i = 0; (i < minOccurs); ++i) {
            final Element child = ElementU.addElementNS(element, name.getLocalPart(), name.getNamespaceURI());
            final DataType dataType = typeInstance.getDataType();
            final String defaultEnumValue = dataType.getDefaultEnumValue();
            if (defaultEnumValue != null) {
                ElementU.setTextContent(child, defaultEnumValue);
            }
            addContent(child, typeInstance);
        }
    }

    @SuppressWarnings("PMD.AssignmentInOperand")
    private void addChoice(final Element element, final ChoiceTypeInstance choiceInstance) {
        int minOccurs = NumberU.toInt(choiceInstance.getMinOccurs(), 1);
        final int maxOccurs = NumberU.toInt(choiceInstance.getMaxOccurs(), 1);
        if (includeOptional) {
            minOccurs = Math.max(minOccurs, 1);
        }
        final Collection<TypeInstance> typeInstances = new ArrayList<TypeInstance>();
        if (maxOccurs == Integer.MAX_VALUE) {
            typeInstances.addAll(choiceInstance.getInstances());
        } else {
            typeInstances.add(choiceInstance.getInstances().iterator().next());
        }
        int count = 0;
        while (count < minOccurs) {
            for (final TypeInstance typeInstance : typeInstances) {
                addAChoice(element, typeInstance);
                if (++count >= maxOccurs) {
                    break;
                }
            }
        }
    }

    private void addAChoice(final Element element, final TypeInstance typeInstance) {
        // recurse
        final TypeInstance.NodeType nodeType = typeInstance.getNodeType();
        if (TypeInstance.NodeType.attribute.equals(nodeType)) {
            addAttribute(element, typeInstance);
        } else if (TypeInstance.NodeType.element.equals(nodeType)) {
            addElement(element, typeInstance);
        } else if (TypeInstance.NodeType.choice.equals(nodeType) && (typeInstance instanceof ChoiceTypeInstance)) {
            addChoice(element, (ChoiceTypeInstance) typeInstance);
        } else {
            throw new IllegalStateException(typeInstance.getDataType().getQName().toString());
        }
    }
}
