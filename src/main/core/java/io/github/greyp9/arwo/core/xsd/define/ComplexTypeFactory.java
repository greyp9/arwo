package io.github.greyp9.arwo.core.xsd.define;

import io.github.greyp9.arwo.core.xed.core.XedU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.atom.XsdAtom;
import io.github.greyp9.arwo.core.xsd.core.XsdAtomU;
import io.github.greyp9.arwo.core.xsd.core.XsdTypeU;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.data.DataTypeRestrictions;
import io.github.greyp9.arwo.core.xsd.data.NodeType;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.ConcreteTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstances;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import io.github.greyp9.arwo.core.xsd.type.TypeComponents;

import javax.xml.namespace.QName;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

@SuppressWarnings({ "PMD.GodClass", "PMD.TooManyMethods" })
public class ComplexTypeFactory {
    private final TypeComponents typeComponents;
    private final TypeDefinitions typeDefinitions;

    public ComplexTypeFactory(final TypeComponents typeComponents, final TypeDefinitions typeDefinitions) {
        this.typeComponents = typeComponents;
        this.typeDefinitions = typeDefinitions;
    }

    public final void create() {
        final Map<String, XsdAtom> complexTypes = typeComponents.getComplexTypes();
        for (final Map.Entry<String, XsdAtom> entry : complexTypes.entrySet()) {
            processComplexType(entry.getKey(), entry.getValue());
        }
    }

    private DataType processComplexType(final String nameString, final XsdAtom complexType) {
        final DataType dataTypeBase = processBaseType(complexType);
        DataType dataType = typeDefinitions.getComplexTypes().get(nameString);
        if (dataType == null) {
            final QName name = QNameU.getQName(nameString);
            final TypeInstances typeInstances = new TypeInstances(name);
            addTypeInstances(complexType, typeInstances);
            if (dataTypeBase != null) {
                typeInstances.getTypeInstances().addAll(dataTypeBase.getInstances());
            }
            final DataTypeRestrictions restrictions = getRestrictions(complexType);
            dataType = new DataType(name, dataTypeBase, restrictions, typeInstances.getTypeInstances());
            typeDefinitions.getComplexTypes().put(nameString, dataType);
        }
        return dataType;
    }

    private DataTypeRestrictions getRestrictions(final XsdAtom complexType) {
        DataTypeRestrictions typeRestrictions = null;
        final Collection<XsdAtom> simpleContents = complexType.getChildren(XsdU.SIMPLE_CONTENT);
        final XsdAtom simpleContent = (simpleContents.isEmpty()) ? null : simpleContents.iterator().next();
        if (simpleContent != null) {
            final Collection<XsdAtom> restrictions = simpleContent.getChildren(XsdU.RESTRICTION);
            final XsdAtom restriction = (restrictions.isEmpty()) ? null : restrictions.iterator().next();
            if (restriction != null) {
                final DataTypeRestrictionsFactory factory = new DataTypeRestrictionsFactory(restriction);
                typeRestrictions = factory.create();
            }
        }
        return typeRestrictions;
    }

    private DataType processBaseType(final XsdAtom complexType) {
        final QName nameBase = getQNameBase(complexType);
        final Map<String, DataType> dataTypesSimple = typeDefinitions.getSimpleTypes();
        final boolean isDataTypeBase = ((nameBase != null) && (dataTypesSimple.containsKey(nameBase.toString())));
        DataType dataTypeBase;
        if (nameBase == null) {
            dataTypeBase = null;
        } else if (isDataTypeBase) {
            dataTypeBase = dataTypesSimple.get(nameBase.toString());
        } else {
            final XsdAtom complexTypeBase = typeComponents.getComplexTypes().get(nameBase.toString());
            dataTypeBase = processComplexType(nameBase.toString(), complexTypeBase);
        }
        return dataTypeBase;
    }

    @SuppressWarnings("PMD.ConfusingTernary")
    private QName getQNameBase(final XsdAtom complexType) {
        QName name = null;  // it is ok to not have a base type
        final Collection<XsdAtom> simpleContent = complexType.getChildren(XsdU.SIMPLE_CONTENT);
        final Collection<XsdAtom> complexContent = complexType.getChildren(XsdU.COMPLEX_CONTENT);
        if (!simpleContent.isEmpty()) {
            name = getQNameBaseExtension(simpleContent.iterator().next().getChildren(XsdU.EXTENSION));
            if (name == null) {
                name = getQNameBaseExtension(simpleContent.iterator().next().getChildren(XsdU.RESTRICTION));
            }
        } else if (!complexContent.isEmpty()) {
            name = getQNameBaseExtension(complexContent.iterator().next().getChildren(XsdU.EXTENSION));
        }
        return name;
    }

    private QName getQNameBaseExtension(final Collection<XsdAtom> extension) {
        QName name = null;
        if (!extension.isEmpty()) {
            final XsdAtom extension1 = extension.iterator().next();
            final String baseType = ElementU.getAttribute(extension1.getElement(), XsdU.BASE);
            name = XsdAtomU.getQName(baseType, extension1);
        }
        return name;
    }

    private void addTypeInstances(final XsdAtom complexType, final TypeInstances typeInstances) {
        for (final XsdAtom atom : complexType.getChildren()) {
            final String name = atom.getElement().getLocalName();
            if (XsdU.COMPLEX_CONTENT.equals(name)) {
                addComplexContent(atom, typeInstances);
            } else if (XsdU.SIMPLE_CONTENT.equals(name)) {
                addSimpleContent(atom, typeInstances);
            } else if (XsdU.CHOICE.equals(name)) {
                addChoice(atom, typeInstances);
            } else if (XsdU.SEQUENCE.equals(name)) {
                addSequence(atom, typeInstances);
            } else if (XsdU.ATTRIBUTE_GROUP.equals(name)) {
                addAttributeGroup(atom, typeInstances);
            } else if (XsdU.ATTRIBUTE.equals(name)) {
                addAttribute(atom, typeInstances);
            } else {
                throw new IllegalStateException(atom.toString());
            }
        }
    }

    private void addComplexContent(final XsdAtom complexContent, final TypeInstances typeInstances) {
        for (final XsdAtom atom : complexContent.getChildren()) {
            final String name = atom.getElement().getLocalName();
            if (XsdU.EXTENSION.equals(name)) {
                addExtension(atom, typeInstances);
            } else {
                throw new IllegalStateException(atom.toString());
            }
        }
    }

    private void addSimpleContent(final XsdAtom simpleContent, final TypeInstances typeInstances) {
        for (final XsdAtom atom : simpleContent.getChildren()) {
            final String name = atom.getElement().getLocalName();
            if (XsdU.EXTENSION.equals(name)) {
                addExtension(atom, typeInstances);
            } else if (XsdU.RESTRICTION.equals(name)) {
                name.getClass();  // ignore
            } else {
                throw new IllegalStateException(atom.toString());
            }
        }
    }

    private void addExtension(final XsdAtom atomExtension, final TypeInstances typeInstances) {
        for (final XsdAtom atom : atomExtension.getChildren()) {
            final String name = atom.getElement().getLocalName();
            if (XsdU.CHOICE.equals(name)) {
                addChoice(atom, typeInstances);
            } else if (XsdU.GROUP.equals(name)) {
                addGroup(atom, typeInstances);
            } else if (XsdU.SEQUENCE.equals(name)) {
                addSequence(atom, typeInstances);
            } else if (XsdU.ATTRIBUTE_GROUP.equals(name)) {
                addAttributeGroup(atom, typeInstances);
            } else if (XsdU.ATTRIBUTE.equals(name)) {
                addAttribute(atom, typeInstances);
            } else {
                throw new IllegalStateException(atom.toString());
            }
        }
    }

    private void addGroup(final XsdAtom atomGroup, final TypeInstances typeInstances) {
        final boolean refInstance = (ElementU.getAttribute(atomGroup.getElement(), XsdU.REF) != null);
        if (refInstance) {
            addRefInstanceGroup(atomGroup, typeInstances);
        } else {
            throw new IllegalStateException(atomGroup.toString());
        }
    }

    private void addRefInstanceGroup(final XsdAtom atomGroup, final TypeInstances typeInstances) {
        final String ref = ElementU.getAttribute(atomGroup.getElement(), XsdU.REF);
        final QName qname = XsdAtomU.getQName(ref, atomGroup);
        final XsdAtom atomRef = typeComponents.getGroups().get(qname.toString());
        if (atomRef == null) {
            throw new IllegalStateException(qname.toString());
        }
        for (final XsdAtom atom1 : atomRef.getChildren()) {
            final String name = atom1.getElement().getLocalName();
            if (XsdU.CHOICE.equals(name)) {
                addChoice(atom1, typeInstances);
            } else if (XsdU.SEQUENCE.equals(name)) {
                addSequence(atom1, typeInstances);
            } else {
                throw new IllegalStateException(atomGroup.toString());
            }
        }
    }

    private void addChoice(final XsdAtom atomChoice, final TypeInstances typeInstances) {
        final String minOccurs = ElementU.getAttribute(atomChoice.getElement(), XsdU.MIN_OCCURS);
        final String maxOccurs = ElementU.getAttribute(atomChoice.getElement(), XsdU.MAX_OCCURS);
        final ChoiceTypeInstance typeInstance = new ChoiceTypeInstance(
                typeInstances.getQNameParent(), minOccurs, maxOccurs);
        final TypeInstances instancesChoice = typeInstance.getTypeInstances();
        for (final XsdAtom atom : atomChoice.getChildren()) {
            final String name = atom.getElement().getLocalName();
            if (XsdU.ELEMENT.equals(name)) {
                addElement(atom, instancesChoice);
            } else if (XsdU.GROUP.equals(name)) {
                addGroup(atom, instancesChoice);
            } else if (XsdU.SEQUENCE.equals(name)) {
                addSequence(atom, instancesChoice);
            } else {
                throw new IllegalStateException(atom.toString());
            }
        }
        typeInstances.getTypeInstances().add(typeInstance);
    }

    private void addSequence(final XsdAtom atomSequence, final TypeInstances typeInstances) {
        for (final XsdAtom atom : atomSequence.getChildren()) {
            final String name = atom.getElement().getLocalName();
            if (XsdU.CHOICE.equals(name)) {
                addChoice(atom, typeInstances);
            } else if (XsdU.GROUP.equals(name)) {
                addGroup(atom, typeInstances);
            } else if (XsdU.ELEMENT.equals(name)) {
                addElement(atom, typeInstances);
            } else if (XsdU.SEQUENCE.equals(name)) {
                addSequence(atom, typeInstances);
            } else {
                throw new IllegalStateException(atom.toString());
            }
        }
    }

    private void addElement(final XsdAtom atom, final TypeInstances typeInstances) {
        final boolean refInstance = (ElementU.getAttribute(atom.getElement(), XsdU.REF) != null);
        final boolean typeInstance = (ElementU.getAttribute(atom.getElement(), XsdU.TYPE) != null);
        final boolean anonInstance = (ElementU.getAttribute(atom.getElement(), XsdU.NAME) != null);
        final String identity = ElementU.getAttributeNS(atom.getElement(), XedU.IDENTITY, XedU.NS_URI_XED);
        if (refInstance) {
            addRefInstance(atom, typeInstances, identity);
        } else if (typeInstance) {
            addTypeInstance(atom, typeInstances, identity);
        } else if (anonInstance) {
            final String name = ElementU.getAttribute(atom.getElement(), XsdU.NAME);
            addTypeInstanceAnonymous(NodeType.element, name, atom, typeInstances);
        } else {
            throw new IllegalStateException(atom.toString());
        }
    }

    private void addAttributeGroup(final XsdAtom attributeGroup, final TypeInstances typeInstances) {
        final String ref = ElementU.getAttribute(attributeGroup.getElement(), XsdU.REF);
        if (ref == null) {
            addAttributes(attributeGroup.getChildren(), typeInstances);
        } else {
            final QName qname = XsdAtomU.getQName(ref, attributeGroup);
            final XsdAtom attributeGroupRef = typeComponents.getAttributeGroups().get(qname.toString());
            addAttributes(attributeGroupRef.getChildren(), typeInstances);
        }
    }

    private void addAttributes(final Collection<XsdAtom> atoms, final TypeInstances typeInstances) {
        for (final XsdAtom atom : atoms) {
            final String name = atom.getElement().getLocalName();
            if (XsdU.ATTRIBUTE.equals(name)) {
                addAttribute(atom, typeInstances);
            } else {
                throw new IllegalStateException(atom.toString());
            }
        }
    }

    @SuppressWarnings("PMD.ConfusingTernary")
    private void addAttribute(final XsdAtom atom, final TypeInstances typeInstances) {
        final String ref = ElementU.getAttribute(atom.getElement(), XsdU.REF);
        final String name = ElementU.getAttribute(atom.getElement(), XsdU.NAME);
        final String type = ElementU.getAttribute(atom.getElement(), XsdU.TYPE,
                XsdTypeU.Const.ANY_SIMPLE_TYPE.getLocalPart());
        final String identity = ElementU.getAttributeNS(atom.getElement(), XedU.IDENTITY, XedU.NS_URI_XED);
        if (ref != null) {
            ref.getClass();
        } else if (type != null) {
            addTypeInstance(atom, typeInstances, identity);
        } else {
            addTypeInstanceAnonymous(NodeType.attribute, name, atom, typeInstances);
        }
    }

    private void addRefInstance(final XsdAtom atom, final TypeInstances typeInstances, final String identity) {
        final String ref = ElementU.getAttribute(atom.getElement(), XsdU.REF);
        final QName qname = XsdAtomU.getQName(ref, atom);
        final XsdAtom atomRef = typeComponents.getElements().get(qname.toString());
        if (atomRef == null) {
            throw new IllegalStateException(qname.toString());
        }
        // from original
        final String name = ElementU.getAttribute(atomRef.getElement(), XsdU.NAME);
        final String type = ElementU.getAttribute(atomRef.getElement(), XsdU.TYPE);
        // from reference
        final String minOccurs = ElementU.getAttribute(atom.getElement(), XsdU.MIN_OCCURS);
        final String maxOccurs = ElementU.getAttribute(atom.getElement(), XsdU.MAX_OCCURS);
        final String use = ElementU.getAttribute(atom.getElement(), XsdU.USE);
        final String defaultValueApp = ElementU.getAttributeNS(atom.getElement(), XsdU.DEFAULT, XedU.NS_URI_XED);
        final String defaultValue = ElementU.getAttribute(atom.getElement(), XsdU.DEFAULT, defaultValueApp);
        // resolve reference
        final QName qnameComplexType = XsdAtomU.getQName(type, atom);
        final XsdAtom complexTypeAtom = typeComponents.getComplexTypes().get(qnameComplexType.toString());
        final DataType dataTypeComplex = processComplexType(qnameComplexType.toString(), complexTypeAtom);
        final ElementFactory elementFactory = new ElementFactory(typeComponents, typeDefinitions);
        elementFactory.processElement(name, atomRef);
        // register type
        final NodeType nodeType = NodeType.valueOf(atom.getElement().getLocalName());
        final QName qnameRef = QNameU.getQName(XsdAtomU.getTargetNamespace(atomRef), name);
        final TypeInstance typeInstance = new ConcreteTypeInstance(
                atomRef, nodeType, qnameRef, dataTypeComplex, null, minOccurs, maxOccurs, use, defaultValue, identity);
        typeInstances.getTypeInstances().add(typeInstance);
    }

    private void addTypeInstance(final XsdAtom atom, final TypeInstances typeInstances, final String identity) {
        final String type = ElementU.getAttribute(atom.getElement(), XsdU.TYPE,
                XsdTypeU.Const.ANY_SIMPLE_TYPE.getLocalPart());
        final QName qname = XsdAtomU.getQName(type, atom);
        final DataType dataTypeSimple = typeDefinitions.getSimpleTypes().get(qname.toString());
        DataType dataTypeComplex = typeDefinitions.getComplexTypes().get(qname.toString());
        final XsdAtom complexTypeAtom = typeComponents.getComplexTypes().get(qname.toString());
        // recursively process child types
        if ((complexTypeAtom != null) && (dataTypeComplex == null)) {
            dataTypeComplex = processComplexType(qname.toString(), complexTypeAtom);
        }
        final DataType dataType = ((dataTypeSimple == null) ? dataTypeComplex : dataTypeSimple);
        if (dataType == null) {
            throw new IllegalStateException(qname.toString());
        }
        final String name = ElementU.getAttribute(atom.getElement(), XsdU.NAME);
        final String minOccurs = ElementU.getAttribute(atom.getElement(), XsdU.MIN_OCCURS);
        final String maxOccurs = ElementU.getAttribute(atom.getElement(), XsdU.MAX_OCCURS);
        final String use = ElementU.getAttribute(atom.getElement(), XsdU.USE);
        final String defaultValueApp = ElementU.getAttributeNS(atom.getElement(), XsdU.DEFAULT, XedU.NS_URI_XED);
        final String defaultValue = ElementU.getAttribute(atom.getElement(), XsdU.DEFAULT, defaultValueApp);
        final String fixedValue = ElementU.getAttribute(atom.getElement(), XsdU.FIXED, defaultValue);
        // add child type
        final NodeType nodeType = NodeType.valueOf(atom.getElement().getLocalName());
        final QName qname1 = QNameU.getQName(XsdAtomU.getTargetNamespace(atom), name);
        final TypeInstance typeInstance = new ConcreteTypeInstance(
                atom, nodeType, qname1, dataType, null, minOccurs, maxOccurs, use, fixedValue, identity);
        typeInstances.getTypeInstances().add(typeInstance);
    }

    @SuppressWarnings("PMD.ConfusingTernary")
    private void addTypeInstanceAnonymous(
            final NodeType nodeType, final String name, final XsdAtom atom, final TypeInstances typeInstances) {
        final QName qname = QNameU.getQName(XsdAtomU.getTargetNamespace(atom), name);
        final Collection<XsdAtom> simpleType = atom.getChildren(XsdU.SIMPLE_TYPE);
        final Collection<XsdAtom> complexType = atom.getChildren(XsdU.COMPLEX_TYPE);
        if (!simpleType.isEmpty()) {
            final XsdAtom atomSimpleType = simpleType.iterator().next();
            final DataType dataType = toDataTypeAnonymousSimpleType(atomSimpleType, typeInstances.getQNameParent());
            typeInstances.getTypeInstances().add(new ConcreteTypeInstance(null, nodeType, qname, dataType));
        } else if (!complexType.isEmpty()) {
            final XsdAtom atomComplexType = complexType.iterator().next();
            final DataType dataType = toDataTypeAnonymousComplexType(atomComplexType, typeInstances.getQNameParent());
            typeInstances.getTypeInstances().add(new ConcreteTypeInstance(null, nodeType, qname, dataType));
        } else {
            throw new IllegalStateException(atom.toString());
        }
    }

    private DataType toDataTypeAnonymousSimpleType(final XsdAtom atom, final QName name) {
        // resolve baseType
        final XsdAtom restriction = atom.getChildren(XsdU.RESTRICTION).iterator().next();
        final String baseType = ElementU.getAttribute(restriction.getElement(), XsdU.BASE);
        final QName nameBase = XsdAtomU.getQName(baseType, atom);
        final DataType dataTypeBase = typeDefinitions.getSimpleTypes().get(nameBase.toString());
        // register anonymous type, and link to type instance
        final Set<String> names = typeDefinitions.getSimpleTypes().keySet();
        final QName nameAnon = getQNameUnique(name, String.format(Const.PATTERN_ANON_NAME, XsdU.SIMPLE_TYPE), names);
        final DataType dataTypeAnon = new DataType(nameAnon, dataTypeBase, null, null);
        typeDefinitions.getSimpleTypes().put(nameAnon.toString(), dataTypeAnon);
        return dataTypeAnon;
    }

    private DataType toDataTypeAnonymousComplexType(final XsdAtom atomAnonymous, final QName qname) {
        // resolve inner type instances
        final TypeInstances typeInstances = new TypeInstances(qname);
        for (final XsdAtom atom : atomAnonymous.getChildren()) {
            final String name = atom.getElement().getLocalName();
            if (XsdU.COMPLEX_CONTENT.equals(name)) {
                addComplexContent(atom, typeInstances);
            } else if (XsdU.CHOICE.equals(name)) {
                addChoice(atom, typeInstances);
            } else if (XsdU.SEQUENCE.equals(name)) {
                addSequence(atom, typeInstances);
            } else if (XsdU.ATTRIBUTE_GROUP.equals(name)) {
                addAttributeGroup(atom, typeInstances);
            } else if (XsdU.ATTRIBUTE.equals(name)) {
                addAttribute(atom, typeInstances);
            } else {
                throw new IllegalStateException(atom.toString());
            }
        }
        // register anonymous type, and link to type instance
        final Set<String> names = typeDefinitions.getComplexTypes().keySet();
        final QName nameAnon = getQNameUnique(qname, String.format(Const.PATTERN_ANON_NAME, XsdU.COMPLEX_TYPE), names);
        final DataType dataTypeAnon = new DataType(nameAnon, null, null, typeInstances.getTypeInstances());
        typeDefinitions.getComplexTypes().put(nameAnon.toString(), dataTypeAnon);
        return dataTypeAnon;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private QName getQNameUnique(final QName qnameParent, final String tag, final Collection<String> names) {
        int i = 0;
        String name = String.format("%s-%s-%d", qnameParent.getLocalPart(), tag, ++i);
        QName qname = new QName(qnameParent.getNamespaceURI(), name);
        while (names.contains(qname.toString())) {
            name = String.format("%s-%s-%d", qnameParent.getLocalPart(), tag, ++i);
            qname = new QName(qnameParent.getNamespaceURI(), name);
        }
        return qname;
    }

    public static class Const {
        private static final String PATTERN_ANON_NAME = "anon-%s";
    }
}
