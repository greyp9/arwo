package io.github.greyp9.arwo.core.xsd.type.test;

import io.github.greyp9.arwo.core.app.test.TestApp;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.xsd.atom.XsdAtom;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollection;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollectionFactory;
import io.github.greyp9.arwo.core.xsd.type.TypeComponents;
import io.github.greyp9.arwo.core.xsd.type.TypeComponentsFactory;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URL;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

public class SchemaTypeAppTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjustShort(Logger.getLogger(""));
    }

    @Test
    public void testAssembleTypeComponents() throws Exception {
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
            TypeComponentsFactory tcFactory = new TypeComponentsFactory(schemaCollection);
            TypeComponents typeComponents = tcFactory.create();
            trace(typeComponents, logger, Level.FINEST);
        }
    }

    public static void trace(final TypeComponents typeComponents, final Logger logger, final Level level) {
        // inspect model
        for (Map.Entry<String, XsdAtom> entry : typeComponents.getElements().entrySet()) {
            logger.log(level, "ELEMENT/" + entry.getKey());
        }
        for (Map.Entry<String, XsdAtom> entry : typeComponents.getComplexTypes().entrySet()) {
            logger.log(level, "COMPLEX_TYPE/" + entry.getKey());
        }
        for (Map.Entry<String, XsdAtom> entry : typeComponents.getSimpleTypes().entrySet()) {
            logger.log(level, "SIMPLE_TYPE/" + entry.getKey());
        }
        for (Map.Entry<String, XsdAtom> entry : typeComponents.getGroups().entrySet()) {
            logger.log(level, "GROUP/" + entry.getKey());
        }
        for (Map.Entry<String, XsdAtom> entry : typeComponents.getAttributeGroups().entrySet()) {
            logger.log(level, "ATTRIBUTE_GROUP/" + entry.getKey());
        }
    }
}
