package io.github.greyp9.arwo.core.xsd.atom;

import io.github.greyp9.arwo.core.xsd.core.XsdU;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;

import java.util.Collection;
import java.util.TreeSet;

public class XsdAtomEnumeration {
    private final Collection<String> atoms;
    private final Collection<String> atomsIgnore;

    public XsdAtomEnumeration() {
        this.atoms = new TreeSet<String>();
        this.atomsIgnore = new TreeSet<String>();
        load();
    }

    public final boolean isChild(final Element parent, final Element child) {
        return atoms.contains(toNameElement(parent, child));
    }

    public final boolean isChildIgnore(final Element parent, final Element child) {
        return atomsIgnore.contains(toNameElement(parent, child));
    }

    public final boolean isAttr(final Element parent, final Attr child) {
        return atoms.contains(toNameAttr(parent, child));
    }

    public final boolean isAttrIgnore(final Element parent, final Attr child) {
        return atomsIgnore.contains(toNameAttr(parent, child));
    }

    @SuppressWarnings("PMD.ExcessiveMethodLength")
    private void load() {
        atomsIgnore.add(toNameAttr(XsdU.ELEMENT, XsdU.ABSTRACT));
        atomsIgnore.add(toNameAttr(XsdU.GROUP, XsdU.MAX_OCCURS));
        atomsIgnore.add(toNameAttr(XsdU.GROUP, XsdU.MIN_OCCURS));
        atomsIgnore.add(toNameAttr(XsdU.SCHEMA, XsdU.ATTRIBUTE_FORM_DEFAULT));
        atomsIgnore.add(toNameAttr(XsdU.SCHEMA, XsdU.ELEMENT_FORM_DEFAULT));
        atomsIgnore.add(toNameAttr(XsdU.SCHEMA, XsdU.VERSION));
        atomsIgnore.add(toNameAttr(XsdU.SEQUENCE, XsdU.MAX_OCCURS));
        atomsIgnore.add(toNameAttr(XsdU.SEQUENCE, XsdU.MIN_OCCURS));
        atomsIgnore.add(toNameElement(XsdU.ELEMENT, XsdU.ANNOTATION));
        atomsIgnore.add(toNameElement(XsdU.SCHEMA, XsdU.ANNOTATION));
        atomsIgnore.add(toNameElement(XsdU.SEQUENCE, XsdU.ANY));
        // SchemaCollectionFactory
        atoms.add(toNameAttr(XsdU.SCHEMA, XsdU.TARGET_NAMESPACE));  // add()
        atoms.add(toNameElement(XsdU.SCHEMA, XsdU.IMPORT));  // doImport()
        atoms.add(toNameAttr(XsdU.IMPORT, XsdU.NAMESPACE));  // doImport()
        atoms.add(toNameAttr(XsdU.IMPORT, XsdU.SCHEMA_LOCATION));  // doImport()
        atoms.add(toNameElement(XsdU.SCHEMA, XsdU.INCLUDE));  // doInclude()
        atoms.add(toNameAttr(XsdU.INCLUDE, XsdU.SCHEMA_LOCATION));  // doInclude()
        // TypeComponentsFactory
        atoms.add(toNameElement(XsdU.SCHEMA, XsdU.ATTRIBUTE_GROUP));
        atoms.add(toNameElement(XsdU.SCHEMA, XsdU.SIMPLE_TYPE));
        atoms.add(toNameElement(XsdU.SCHEMA, XsdU.GROUP));
        atoms.add(toNameElement(XsdU.SCHEMA, XsdU.COMPLEX_TYPE));
        atoms.add(toNameElement(XsdU.SCHEMA, XsdU.ELEMENT));
        atoms.add(toNameAttr(XsdU.ATTRIBUTE_GROUP, XsdU.NAME));
        atoms.add(toNameAttr(XsdU.SIMPLE_TYPE, XsdU.NAME));
        atoms.add(toNameAttr(XsdU.GROUP, XsdU.NAME));
        atoms.add(toNameAttr(XsdU.COMPLEX_TYPE, XsdU.NAME));
        atoms.add(toNameAttr(XsdU.ELEMENT, XsdU.NAME));
        // SimpleTypeFactory
        atoms.add(toNameElement(XsdU.SIMPLE_TYPE, XsdU.RESTRICTION));
        atoms.add(toNameAttr(XsdU.RESTRICTION, XsdU.BASE));
        // DataTypeRestrictionsFactory
        atoms.add(toNameElement(XsdU.RESTRICTION, XsdU.ENUMERATION));
        atoms.add(toNameElement(XsdU.RESTRICTION, XsdU.PATTERN));
        atoms.add(toNameElement(XsdU.RESTRICTION, XsdU.MIN_INCLUSIVE));
        atoms.add(toNameElement(XsdU.RESTRICTION, XsdU.MAX_INCLUSIVE));
        atoms.add(toNameElement(XsdU.RESTRICTION, XsdU.MIN_LENGTH));
        atoms.add(toNameElement(XsdU.RESTRICTION, XsdU.MAX_LENGTH));
        atoms.add(toNameAttr(XsdU.ENUMERATION, XsdU.VALUE));
        atoms.add(toNameAttr(XsdU.PATTERN, XsdU.VALUE));
        atoms.add(toNameAttr(XsdU.MIN_INCLUSIVE, XsdU.VALUE));
        atoms.add(toNameAttr(XsdU.MAX_INCLUSIVE, XsdU.VALUE));
        atoms.add(toNameAttr(XsdU.MIN_LENGTH, XsdU.VALUE));
        atoms.add(toNameAttr(XsdU.MAX_LENGTH, XsdU.VALUE));
        // ComplexTypeFactory
        atoms.add(toNameElement(XsdU.RESTRICTION, XsdU.MAX_LENGTH));
        atoms.add(toNameElement(XsdU.COMPLEX_TYPE, XsdU.SIMPLE_CONTENT));
        atoms.add(toNameElement(XsdU.COMPLEX_TYPE, XsdU.COMPLEX_CONTENT));
        atoms.add(toNameElement(XsdU.SIMPLE_CONTENT, XsdU.EXTENSION));
        atoms.add(toNameElement(XsdU.COMPLEX_CONTENT, XsdU.EXTENSION));
        atoms.add(toNameAttr(XsdU.EXTENSION, XsdU.BASE));
        atoms.add(toNameElement(XsdU.COMPLEX_TYPE, XsdU.COMPLEX_CONTENT));
        atoms.add(toNameElement(XsdU.COMPLEX_TYPE, XsdU.SIMPLE_CONTENT));
        atoms.add(toNameElement(XsdU.COMPLEX_TYPE, XsdU.CHOICE));
        atoms.add(toNameElement(XsdU.COMPLEX_TYPE, XsdU.SEQUENCE));
        atoms.add(toNameElement(XsdU.COMPLEX_TYPE, XsdU.ATTRIBUTE_GROUP));
        atoms.add(toNameElement(XsdU.COMPLEX_TYPE, XsdU.ATTRIBUTE));
        atoms.add(toNameElement(XsdU.COMPLEX_CONTENT, XsdU.EXTENSION));
        atoms.add(toNameElement(XsdU.SIMPLE_CONTENT, XsdU.EXTENSION));
        atoms.add(toNameElement(XsdU.EXTENSION, XsdU.CHOICE));
        atoms.add(toNameElement(XsdU.EXTENSION, XsdU.GROUP));
        atoms.add(toNameElement(XsdU.EXTENSION, XsdU.SEQUENCE));
        atoms.add(toNameElement(XsdU.EXTENSION, XsdU.ATTRIBUTE_GROUP));
        atoms.add(toNameElement(XsdU.EXTENSION, XsdU.ATTRIBUTE));
        atoms.add(toNameAttr(XsdU.GROUP, XsdU.REF));
        atoms.add(toNameElement(XsdU.GROUP, XsdU.CHOICE));
        atoms.add(toNameElement(XsdU.GROUP, XsdU.SEQUENCE));
        atoms.add(toNameElement(XsdU.CHOICE, XsdU.ELEMENT));
        atoms.add(toNameAttr(XsdU.CHOICE, XsdU.MIN_OCCURS));
        atoms.add(toNameAttr(XsdU.CHOICE, XsdU.MAX_OCCURS));
        atoms.add(toNameElement(XsdU.SEQUENCE, XsdU.CHOICE));
        atoms.add(toNameElement(XsdU.SEQUENCE, XsdU.GROUP));
        atoms.add(toNameElement(XsdU.SEQUENCE, XsdU.ELEMENT));
        atoms.add(toNameAttr(XsdU.ELEMENT, XsdU.REF));
        atoms.add(toNameAttr(XsdU.ELEMENT, XsdU.TYPE));
        atoms.add(toNameAttr(XsdU.ELEMENT, XsdU.NAME));
        // addAttributeGroup
        atoms.add(toNameAttr(XsdU.ATTRIBUTE_GROUP, XsdU.REF));
        atoms.add(toNameElement(XsdU.ATTRIBUTE_GROUP, XsdU.ATTRIBUTE));
        // addAttribute
        atoms.add(toNameAttr(XsdU.ATTRIBUTE, XsdU.NAME));
        atoms.add(toNameAttr(XsdU.ATTRIBUTE, XsdU.TYPE));
        // addRefInstance / addTypeInstance
        atoms.add(toNameAttr(XsdU.ELEMENT, XsdU.MIN_OCCURS));
        atoms.add(toNameAttr(XsdU.ELEMENT, XsdU.MAX_OCCURS));
        atoms.add(toNameAttr(XsdU.ELEMENT, XsdU.USE));
        atoms.add(toNameAttr(XsdU.ELEMENT, XsdU.DEFAULT));
        atoms.add(toNameAttr(XsdU.ATTRIBUTE, XsdU.MIN_OCCURS));
        atoms.add(toNameAttr(XsdU.ATTRIBUTE, XsdU.MAX_OCCURS));
        atoms.add(toNameAttr(XsdU.ATTRIBUTE, XsdU.USE));
        atoms.add(toNameAttr(XsdU.ATTRIBUTE, XsdU.DEFAULT));
        // addTypeInstanceAnonymous
        atoms.add(toNameElement(XsdU.ATTRIBUTE, XsdU.SIMPLE_TYPE));
        atoms.add(toNameElement(XsdU.ELEMENT, XsdU.SIMPLE_TYPE));
        atoms.add(toNameElement(XsdU.ELEMENT, XsdU.COMPLEX_TYPE));
        // ElementFactory
        atoms.add(toNameAttr(XsdU.ELEMENT, XsdU.NAME));
        atoms.add(toNameAttr(XsdU.ELEMENT, XsdU.TYPE));
        atoms.add(toNameAttr(XsdU.ELEMENT, XsdU.SUBSTITUTION_GROUP));
    }

    private static String toNameElement(final Element parent, final Element child) {
        return toNameElement(parent.getLocalName(), child.getLocalName());
    }

    private static String toNameElement(final String parent, final String child) {
        return String.format("[%s][%s]", parent, child);
    }

    private static String toNameAttr(final Element parent, final Attr child) {
        return toNameAttr(parent.getLocalName(), child.getLocalName());
    }

    private static String toNameAttr(final String parent, final String child) {
        return String.format("[%s][@%s]", parent, child);
    }
}
