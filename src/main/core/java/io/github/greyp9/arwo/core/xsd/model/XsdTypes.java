package io.github.greyp9.arwo.core.xsd.model;

import io.github.greyp9.arwo.core.xpath.XPathContext;
import io.github.greyp9.arwo.core.xpath.XPathContextFactory;
import io.github.greyp9.arwo.core.xsd.atom.SchemaAtom;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollection;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollectionFactory;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitionsFactory;
import io.github.greyp9.arwo.core.xsd.type.TypeComponents;
import io.github.greyp9.arwo.core.xsd.type.TypeComponentsFactory;

import java.io.IOException;
import java.net.URL;
import java.util.Collection;

public class XsdTypes {
    private final URL urlInitial;
    private final URL urlCatalog;
    private final TypeDefinitions typeDefinitions;
    private final XPathContext context;

    public final URL getUrlInitial() {
        return urlInitial;
    }

    public final URL getUrlCatalog() {
        return urlCatalog;
    }

    public final TypeDefinitions getTypeDefinitions() {
        return typeDefinitions;
    }

    public final XPathContext getContext() {
        return context;
    }

    public XsdTypes(final URL urlInitial) throws IOException {
        this(urlInitial, null, null);
    }

    public XsdTypes(final URL urlInitial, final URL urlCatalog, final URL urlTransform) throws IOException {
        this.urlInitial = urlInitial;
        this.urlCatalog = urlCatalog;
        this.typeDefinitions = createTypeDefinitions(urlInitial, urlCatalog, urlTransform);
        this.context = createContext(typeDefinitions);
    }

    private static TypeDefinitions createTypeDefinitions(
            final URL urlInitial, final URL urlCatalog, final URL urlTransform) throws IOException {
        final SchemaCollectionFactory scFactory = new SchemaCollectionFactory(urlCatalog, urlTransform);
        final SchemaCollection schemaCollection = scFactory.create(urlInitial);
        // pre-parse of schema collection
        final TypeComponentsFactory tcFactory = new TypeComponentsFactory(schemaCollection);
        final TypeComponents typeComponents = tcFactory.create();
        // create type definitions for schema set
        final TypeDefinitionsFactory tdFactory = new TypeDefinitionsFactory(typeComponents);
        return tdFactory.create();
    }

    private static XPathContext createContext(final TypeDefinitions typeDefinitions) throws IOException {
        final XPathContext context = new XPathContext();
        final SchemaCollection schemaCollection = typeDefinitions.getTypeComponents().getSchemaCollection();
        final Collection<SchemaAtom> schemaElements = schemaCollection.getSchemas().values();
        for (final SchemaAtom schemaElement : schemaElements) {
            XPathContextFactory.update(context, schemaElement.getAtom().getElement());
        }
        return context;
    }
}
