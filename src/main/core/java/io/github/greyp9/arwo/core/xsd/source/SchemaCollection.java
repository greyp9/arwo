package io.github.greyp9.arwo.core.xsd.source;

import io.github.greyp9.arwo.core.xsd.atom.SchemaAtom;

import java.util.Map;

public class SchemaCollection {
    private final SchemaAtom schemaInitial;
    private final Map<String, SchemaAtom> schemas;

    public final SchemaAtom getSchemaInitial() {
        return schemaInitial;
    }

    public final Map<String, SchemaAtom> getSchemas() {
        return schemas;
    }

    public SchemaCollection(final SchemaAtom schemaInitial, final Map<String, SchemaAtom> schemas) {
        this.schemaInitial = schemaInitial;
        this.schemas = schemas;
    }
}
