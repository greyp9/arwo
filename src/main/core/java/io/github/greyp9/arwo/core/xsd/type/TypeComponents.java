package io.github.greyp9.arwo.core.xsd.type;

import io.github.greyp9.arwo.core.xsd.atom.XsdAtom;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollection;

import java.util.Map;
import java.util.TreeMap;

public class TypeComponents {
    private final SchemaCollection schemaCollection;
    private final Map<String, XsdAtom> attributeGroups;
    private final Map<String, XsdAtom> simpleTypes;
    private final Map<String, XsdAtom> groups;
    private final Map<String, XsdAtom> complexTypes;
    private final Map<String, XsdAtom> elements;

    public final SchemaCollection getSchemaCollection() {
        return schemaCollection;
    }

    public final Map<String, XsdAtom> getAttributeGroups() {
        return attributeGroups;
    }

    public final Map<String, XsdAtom> getSimpleTypes() {
        return simpleTypes;
    }

    public final Map<String, XsdAtom> getGroups() {
        return groups;
    }

    public final Map<String, XsdAtom> getComplexTypes() {
        return complexTypes;
    }

    public final Map<String, XsdAtom> getElements() {
        return elements;
    }

    public TypeComponents(final SchemaCollection schemaCollection) {
        this.schemaCollection = schemaCollection;
        this.attributeGroups = new TreeMap<String, XsdAtom>();
        this.simpleTypes = new TreeMap<String, XsdAtom>();
        this.groups = new TreeMap<String, XsdAtom>();
        this.complexTypes = new TreeMap<String, XsdAtom>();
        this.elements = new TreeMap<String, XsdAtom>();
    }
}
