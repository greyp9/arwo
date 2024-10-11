package io.github.greyp9.arwo.core.xsd.document;

import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import org.w3c.dom.Document;

public final class DocumentFactoryU {

    private DocumentFactoryU() {
    }

    public static Document generateDocument(
            final XsdTypes xsdTypes, final TypeInstance typeInstance, final boolean includeOptional) {
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final DocumentFactory documentFactory = new DocumentFactory(typeDefinitions, includeOptional);
        return documentFactory.generateEmpty(typeInstance);
    }
}
