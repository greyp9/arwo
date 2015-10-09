package io.github.greyp9.arwo.core.xsd.factory;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.atom.XsdAtom;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

public final class XsdAtomFactory {

    private XsdAtomFactory() {
    }

    public static XsdAtom create(final Element element, final XsdAtom parent) throws IOException {
        final XsdAtom child = new XsdAtom(element, parent);
        final String name = element.getLocalName();
        for (final AtomDescriptor atom : Const.ATOMS) {
            if (name.equals(atom.getName())) {
                load(child, atom);
            }
        }
        return child;
    }

    private static void load(final XsdAtom atom, final AtomDescriptor descriptor) throws IOException {
        final Element element = atom.getElement();
        final Collection<Element> children = ElementU.getChildren(element);
        for (final Element child : children) {
            loadChild(atom, child, descriptor.getChildNames());
        }
        final Collection<Attr> attrs = ElementU.getAttributes(element);
        for (final Attr attr : attrs) {
            loadAttribute(atom, attr, descriptor.getAttrNames());
        }
    }

    private static void loadChild(
            final XsdAtom atom, final Element child, final Collection<String> names) throws IOException {
        final Element element = atom.getElement();
        if (names.contains(child.getLocalName())) {
            atom.addChild(create(child, atom));
        } else {
            throw new IOException(String.format("[%s]/[%s]", QNameU.getQName(element), QNameU.getQName(child)));
        }
    }

    private static void loadAttribute(
            final XsdAtom atom, final Attr attr, final Collection<String> names) throws IOException {
        final Element element = atom.getElement();
        final String uri = ((attr.getNamespaceURI() == null) ? element.getNamespaceURI() : attr.getNamespaceURI());
        final NameTypeValue nameTypeValue = new NameTypeValue(attr.getLocalName(), uri, attr.getValue());
        if (XsdU.NS_URI_XML.equals(nameTypeValue.getType())) {
            atom.addNamespace(new NameTypeValue(nameTypeValue.getName(), null, nameTypeValue.getValueS()));
        } else if ((XsdU.NS_URI_XSD.equals(nameTypeValue.getType())) && (names.contains(nameTypeValue.getName()))) {
            atom.addAttribute(nameTypeValue);
        } else if (XsdU.NS_URI_XED.equals(nameTypeValue.getType())) {
            atom.addDirective(nameTypeValue);
        } else {
            throw new IOException(String.format("[%s]/@[%s]", QNameU.getQName(element), QNameU.getQName(attr)));
        }
    }

    private static class AtomDescriptor {
        private final String name;
        private final Collection<String> childNames;
        private final Collection<String> attrNames;

        public AtomDescriptor(final String name, final String[] childNames, final String[] attrNames) {  // NOPMD
            this.name = name;
            this.childNames = Arrays.asList(childNames);
            this.attrNames = Arrays.asList(attrNames);
        }

        public String getName() {
            return name;
        }

        public Collection<String> getChildNames() {
            return childNames;
        }

        public Collection<String> getAttrNames() {
            return attrNames;
        }
    }

    private static class Const {
        private static final String[] ANY_E = {};
        private static final String[] ANY_A = {};
        private static final String[] ATTRIBUTE_E = {};
        private static final String[] ATTRIBUTE_A = { XsdU.DEFAULT, XsdU.NAME, XsdU.TYPE, XsdU.USE };
        private static final String[] ATTRIBUTE_GROUP_E = { XsdU.ATTRIBUTE };
        private static final String[] ATTRIBUTE_GROUP_A = { XsdU.NAME, XsdU.REF };
        private static final String[] CHOICE_E = { XsdU.ELEMENT };
        private static final String[] CHOICE_A = { XsdU.MAX_OCCURS, XsdU.MIN_OCCURS };
        private static final String[] COMPLEX_CONTENT_E = { XsdU.EXTENSION };
        private static final String[] COMPLEX_CONTENT_A = {};
        private static final String[] COMPLEX_TYPE_E = {
                XsdU.ATTRIBUTE, XsdU.ATTRIBUTE_GROUP, XsdU.CHOICE, XsdU.COMPLEX_CONTENT, XsdU.SEQUENCE };
        private static final String[] COMPLEX_TYPE_A = { XsdU.NAME };
        private static final String[] ELEMENT_E = { XsdU.COMPLEX_TYPE };
        private static final String[] ELEMENT_A = {
                XsdU.ABSTRACT, XsdU.DEFAULT, XsdU.MAX_OCCURS, XsdU.MIN_OCCURS, XsdU.NAME, XsdU.REF,
                XsdU.SUBSTITUTION_GROUP, XsdU.TYPE };
        private static final String[] ENUMERATION_E = {};
        private static final String[] ENUMERATION_A = { XsdU.VALUE };
        private static final String[] EXTENSION_E = { XsdU.ATTRIBUTE, XsdU.CHOICE, XsdU.SEQUENCE };
        private static final String[] EXTENSION_A = { XsdU.BASE, XsdU.SUBSTITUTION_GROUP };
        private static final String[] GROUP_E = { XsdU.CHOICE };
        private static final String[] GROUP_A = { XsdU.NAME, XsdU.REF };
        private static final String[] IMPORT_E = {};
        private static final String[] IMPORT_A = { XsdU.NAMESPACE, XsdU.SCHEMA_LOCATION };
        private static final String[] INCLUDE_E = {};
        private static final String[] INCLUDE_A = { XsdU.SCHEMA_LOCATION };
        private static final String[] MAX_INCLUSIVE_E = {};
        private static final String[] MAX_INCLUSIVE_A = { XsdU.VALUE };
        private static final String[] MAX_LENGTH_E = {};
        private static final String[] MAX_LENGTH_A = { XsdU.VALUE };
        private static final String[] MIN_INCLUSIVE_E = {};
        private static final String[] MIN_INCLUSIVE_A = { XsdU.VALUE };
        private static final String[] MIN_LENGTH_E = {};
        private static final String[] MIN_LENGTH_A = { XsdU.VALUE };
        private static final String[] PATTERN_E = {};
        private static final String[] PATTERN_A = { XsdU.VALUE };
        private static final String[] RESTRICTION_E = {
                XsdU.ENUMERATION, XsdU.MAX_INCLUSIVE, XsdU.MAX_LENGTH, XsdU.MIN_INCLUSIVE, XsdU.MIN_LENGTH,
                XsdU.PATTERN };
        private static final String[] RESTRICTION_A = { XsdU.BASE };
        private static final String[] SCHEMA_E = {
                XsdU.ANNOTATION, XsdU.ATTRIBUTE_GROUP, XsdU.COMPLEX_TYPE, XsdU.ELEMENT, XsdU.GROUP, XsdU.IMPORT,
                XsdU.INCLUDE, XsdU.SIMPLE_TYPE };
        private static final String[] SCHEMA_A = { XsdU.ELEMENT_FORM_DEFAULT, XsdU.TARGET_NAMESPACE, XsdU.VERSION };
        private static final String[] SEQUENCE_E = { XsdU.CHOICE, XsdU.ELEMENT, XsdU.GROUP };
        private static final String[] SEQUENCE_A = {};
        private static final String[] SIMPLE_CONTENT_E = {};
        private static final String[] SIMPLE_CONTENT_A = {};
        private static final String[] SIMPLE_TYPE_E = { XsdU.RESTRICTION };
        private static final String[] SIMPLE_TYPE_A = { XsdU.NAME };

        private static final AtomDescriptor ANY = new AtomDescriptor(
                XsdU.ANY, ANY_E, ANY_A);
        private static final AtomDescriptor ATTRIBUTE = new AtomDescriptor(
                XsdU.ATTRIBUTE, ATTRIBUTE_E, ATTRIBUTE_A);
        private static final AtomDescriptor ATTRIBUTE_GROUP = new AtomDescriptor(
                XsdU.ATTRIBUTE_GROUP, ATTRIBUTE_GROUP_E, ATTRIBUTE_GROUP_A);
        private static final AtomDescriptor CHOICE = new AtomDescriptor(
                XsdU.CHOICE, CHOICE_E, CHOICE_A);
        private static final AtomDescriptor COMPLEX_CONTENT = new AtomDescriptor(
                XsdU.COMPLEX_CONTENT, COMPLEX_CONTENT_E, COMPLEX_CONTENT_A);
        private static final AtomDescriptor COMPLEX_TYPE = new AtomDescriptor(
                XsdU.COMPLEX_TYPE, COMPLEX_TYPE_E, COMPLEX_TYPE_A);
        private static final AtomDescriptor ELEMENT = new AtomDescriptor(
                XsdU.ELEMENT, ELEMENT_E, ELEMENT_A);
        private static final AtomDescriptor ENUMERATION = new AtomDescriptor(
                XsdU.ENUMERATION, ENUMERATION_E, ENUMERATION_A);
        private static final AtomDescriptor EXTENSION = new AtomDescriptor(
                XsdU.EXTENSION, EXTENSION_E, EXTENSION_A);
        private static final AtomDescriptor GROUP = new AtomDescriptor(
                XsdU.GROUP, GROUP_E, GROUP_A);
        private static final AtomDescriptor IMPORT = new AtomDescriptor(
                XsdU.IMPORT, IMPORT_E, IMPORT_A);
        private static final AtomDescriptor INCLUDE = new AtomDescriptor(
                XsdU.INCLUDE, INCLUDE_E, INCLUDE_A);
        private static final AtomDescriptor MAX_INCLUSIVE = new AtomDescriptor(
                XsdU.MAX_INCLUSIVE, MAX_INCLUSIVE_E, MAX_INCLUSIVE_A);
        private static final AtomDescriptor MAX_LENGTH = new AtomDescriptor(
                XsdU.MAX_LENGTH, MAX_LENGTH_E, MAX_LENGTH_A);
        private static final AtomDescriptor MIN_INCLUSIVE = new AtomDescriptor(
                XsdU.MIN_INCLUSIVE, MIN_INCLUSIVE_E, MIN_INCLUSIVE_A);
        private static final AtomDescriptor MIN_LENGTH = new AtomDescriptor(
                XsdU.MIN_LENGTH, MIN_LENGTH_E, MIN_LENGTH_A);
        private static final AtomDescriptor PATTERN = new AtomDescriptor(
                XsdU.PATTERN, PATTERN_E, PATTERN_A);
        private static final AtomDescriptor RESTRICTION = new AtomDescriptor(
                XsdU.RESTRICTION, RESTRICTION_E, RESTRICTION_A);
        private static final AtomDescriptor SCHEMA = new AtomDescriptor(
                XsdU.SCHEMA, SCHEMA_E, SCHEMA_A);
        private static final AtomDescriptor SEQUENCE = new AtomDescriptor(
                XsdU.SEQUENCE, SEQUENCE_E, SEQUENCE_A);
        private static final AtomDescriptor SIMPLE_CONTENT = new AtomDescriptor(
                XsdU.SIMPLE_CONTENT, SIMPLE_CONTENT_E, SIMPLE_CONTENT_A);
        private static final AtomDescriptor SIMPLE_TYPE = new AtomDescriptor(
                XsdU.SIMPLE_TYPE, SIMPLE_TYPE_E, SIMPLE_TYPE_A);

        private static final AtomDescriptor[] ATOMS = {
                ANY, ATTRIBUTE, ATTRIBUTE_GROUP, CHOICE, COMPLEX_CONTENT, COMPLEX_TYPE, ELEMENT, ENUMERATION,
                EXTENSION, GROUP, IMPORT, INCLUDE, MAX_INCLUSIVE, MAX_LENGTH, MIN_INCLUSIVE, MIN_LENGTH, PATTERN,
                RESTRICTION, SCHEMA, SEQUENCE, SIMPLE_CONTENT, SIMPLE_TYPE };
    }
}
