package io.github.greyp9.arwo.core.xsd.define;

import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.atom.XsdAtom;
import io.github.greyp9.arwo.core.xsd.core.XsdAtomU;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.data.NodeType;
import io.github.greyp9.arwo.core.xsd.instance.ConcreteTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import io.github.greyp9.arwo.core.xsd.type.TypeComponents;

import javax.xml.namespace.QName;
import java.util.Map;

public class ElementFactory {
    private final TypeComponents typeComponents;
    private final TypeDefinitions typeDefinitions;

    public ElementFactory(final TypeComponents typeComponents, final TypeDefinitions typeDefinitions) {
        this.typeComponents = typeComponents;
        this.typeDefinitions = typeDefinitions;
    }

    public final void create() {
        final Map<String, XsdAtom> elements = typeComponents.getElements();
        for (final Map.Entry<String, XsdAtom> entry : elements.entrySet()) {
            processElement(entry.getKey(), entry.getValue());
        }
    }

    @SuppressWarnings("PMD.ConfusingTernary")
    public final void processElement(final String nameString, final XsdAtom element) {
        final String name = ElementU.getAttribute(element.getElement(), XsdU.NAME);
        final String type = ElementU.getAttribute(element.getElement(), XsdU.TYPE);
        final String substitutionGroup = ElementU.getAttribute(element.getElement(), XsdU.SUBSTITUTION_GROUP);
        // resolve QName associated with data type
        QName qname;
        if (type != null) {
            qname = XsdAtomU.getQName(type, element);
        } else if (substitutionGroup != null) {
            qname = XsdAtomU.getQName(substitutionGroup, element);
            if (!typeDefinitions.getElementTypes().containsKey(qname.toString())) {
                final XsdAtom elementBase = typeComponents.getElements().get(qname.toString());
                processElement(qname.toString(), elementBase);
            }
            final TypeInstance typeInstanceBase = typeDefinitions.getElementTypes().get(qname.toString());
            qname = typeInstanceBase.getDataType().getQName();
        } else {
            throw new IllegalStateException(String.format("[%s]", name));
        }
        // resolve data type for QName
        DataType dataType = typeDefinitions.getSimpleTypes().get(qname.toString());
        if (dataType == null) {
            dataType = typeDefinitions.getComplexTypes().get(qname.toString());
        }
        // bind element name to data type
        if (dataType == null) {
            throw new IllegalStateException(nameString);
        } else {
            final QName qname1 = QNameU.getQName(XsdAtomU.getTargetNamespace(element), name);
            final TypeInstance typeInstance = new ConcreteTypeInstance(element, NodeType.element, qname1, dataType);
            typeDefinitions.getElementTypes().put(nameString, typeInstance);
        }
    }
}
