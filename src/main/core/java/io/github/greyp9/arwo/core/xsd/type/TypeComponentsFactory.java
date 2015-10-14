package io.github.greyp9.arwo.core.xsd.type;

import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.atom.SchemaAtom;
import io.github.greyp9.arwo.core.xsd.atom.XsdAtom;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollection;

import javax.xml.namespace.QName;
import java.util.Collection;
import java.util.Map;

public class TypeComponentsFactory {
    private final SchemaCollection schemaCollection;
    private final TypeComponents typeComponents;

    public TypeComponentsFactory(final SchemaCollection schemaCollection) {
        this.schemaCollection = schemaCollection;
        this.typeComponents = new TypeComponents(schemaCollection);
    }

    public final TypeComponents create() {
        for (final Map.Entry<String, SchemaAtom> entry : schemaCollection.getSchemas().entrySet()) {
            add(entry.getValue());
        }
        return typeComponents;
    }

    private void add(final SchemaAtom schemaAtom) {
        final XsdAtom atom = schemaAtom.getAtom();
        final String targetNamespace = ElementU.getAttribute(atom.getElement(), XsdU.TARGET_NAMESPACE);
        addAttributeGroup(atom.getChildren(XsdU.ATTRIBUTE_GROUP), targetNamespace);
        addSimpleType(atom.getChildren(XsdU.SIMPLE_TYPE), targetNamespace);
        addGroup(atom.getChildren(XsdU.GROUP), targetNamespace);
        addComplexType(atom.getChildren(XsdU.COMPLEX_TYPE), targetNamespace);
        addElement(atom.getChildren(XsdU.ELEMENT), targetNamespace);
    }

    private void addAttributeGroup(final Collection<XsdAtom> atoms, final String targetNamespace) {
        for (final XsdAtom atom : atoms) {
            final String nameString = ElementU.getAttribute(atom.getElement(), XsdU.NAME);
            final QName name = QNameU.getQName(targetNamespace, nameString, null);
            typeComponents.getAttributeGroups().put(name.toString(), atom);
        }
    }

    private void addSimpleType(final Collection<XsdAtom> atoms, final String targetNamespace) {
        for (final XsdAtom atom : atoms) {
            final String nameString = ElementU.getAttribute(atom.getElement(), XsdU.NAME);
            final QName name = QNameU.getQName(targetNamespace, nameString, null);
            typeComponents.getSimpleTypes().put(name.toString(), atom);
        }
    }

    private void addGroup(final Collection<XsdAtom> atoms, final String targetNamespace) {
        for (final XsdAtom atom : atoms) {
            final String nameString = ElementU.getAttribute(atom.getElement(), XsdU.NAME);
            final QName name = QNameU.getQName(targetNamespace, nameString, null);
            typeComponents.getGroups().put(name.toString(), atom);
        }
    }

    private void addComplexType(final Collection<XsdAtom> atoms, final String targetNamespace) {
        for (final XsdAtom atom : atoms) {
            final String nameString = ElementU.getAttribute(atom.getElement(), XsdU.NAME);
            final QName name = QNameU.getQName(targetNamespace, nameString, null);
            typeComponents.getComplexTypes().put(name.toString(), atom);
        }
    }

    private void addElement(final Collection<XsdAtom> atoms, final String targetNamespace) {
        for (final XsdAtom atom : atoms) {
            final String nameString = ElementU.getAttribute(atom.getElement(), XsdU.NAME);
            final QName name = QNameU.getQName(targetNamespace, nameString, null);
            typeComponents.getElements().put(name.toString(), atom);
        }
    }
}
