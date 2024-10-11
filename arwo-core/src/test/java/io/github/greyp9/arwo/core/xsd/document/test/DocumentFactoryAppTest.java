package io.github.greyp9.arwo.core.xsd.document.test;

import io.github.greyp9.arwo.core.app.test.TestApp;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollection;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollectionFactory;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitionsFactory;
import io.github.greyp9.arwo.core.xsd.type.TypeComponents;
import io.github.greyp9.arwo.core.xsd.type.TypeComponentsFactory;
import io.github.greyp9.arwo.core.xsd.validate.Validator;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;

import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

public class DocumentFactoryAppTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testGenerateDocuments() throws Exception {
        for (String xsd : TestApp.Resources.XSD_ARRAY) {
            URL urlInitial = ResourceU.resolve(xsd);
            Assertions.assertNotNull(urlInitial);
            logger.finest("UrlInitial/" + URLCodec.toExternalForm(urlInitial));
            URL urlCatalog = URLCodec.resolve(urlInitial, ".");
            Assertions.assertNotNull(urlInitial);
            logger.finest("UrlCatalog/" + URLCodec.toExternalForm(urlCatalog));
            // resolve schema collection for this initial schema
            SchemaCollectionFactory schemaCollectionFactory = new SchemaCollectionFactory(urlCatalog);
            SchemaCollection schemaCollection = schemaCollectionFactory.create(urlInitial);
            Assertions.assertNotNull(schemaCollection);
            // pre-parse of schema collection
            TypeComponentsFactory typeComponentsFactory = new TypeComponentsFactory(schemaCollection);
            TypeComponents typeComponents = typeComponentsFactory.create();
            Assertions.assertNotNull(typeComponents);
            // create type definitions for schema set
            TypeDefinitionsFactory typeDefinitionsFactory = new TypeDefinitionsFactory(typeComponents);
            TypeDefinitions typeDefinitions = typeDefinitionsFactory.create();
            Assertions.assertNotNull(typeDefinitions);
            boolean[] includeOptionals = { false, true };
            for (boolean includeOptional : includeOptionals) {
                DocumentFactory documentFactory = new DocumentFactory(typeDefinitions, includeOptional);
                for (TypeInstance typeInstance : typeDefinitions.getElementTypes().values()) {
                    Document document = documentFactory.generateEmpty(typeInstance.getQName());
                    logger.log(Level.FINEST, DocumentU.toString(document));
                    // validation
                    validate(urlInitial, null, document, logger);
                }
            }
        }
    }

    public static int validate(final URL urlInitial, final byte[] xsdInitial,
                               final Document document, final Logger logger) throws IOException {
        Validator validator = (xsdInitial == null)
                ? new Validator(urlInitial) : new Validator(urlInitial, xsdInitial);
        Collection<String> messages = validator.validate(DocumentU.toXml(document));
        Level level = (messages.isEmpty() ? Level.FINE : Level.WARNING);
        logger.log(level, "" + messages.size());
        for (String message : messages) {
            logger.warning(message);
        }
        return messages.size();
    }
}
