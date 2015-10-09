package io.github.greyp9.arwo.core.xsd.source;

import io.github.greyp9.arwo.core.xsd.atom.SchemaAtom;

import java.util.Map;

public class SchemaCollection {
    private final String targetNamespace;
    private final Map<String, SchemaAtom> schemas;

    public final String getTargetNamespace() {
        return targetNamespace;
    }

    public final Map<String, SchemaAtom> getSchemas() {
        return schemas;
    }

    public SchemaCollection(final String targetNamespace, final Map<String, SchemaAtom> schemas) {
        this.targetNamespace = targetNamespace;
        this.schemas = schemas;
    }
}
