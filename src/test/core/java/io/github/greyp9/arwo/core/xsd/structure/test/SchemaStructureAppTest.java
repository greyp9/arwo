package io.github.greyp9.arwo.core.xsd.structure.test;

import io.github.greyp9.arwo.core.app.test.TestApp;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollection;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollectionFactory;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitionsFactory;
import io.github.greyp9.arwo.core.xsd.type.TypeComponents;
import io.github.greyp9.arwo.core.xsd.type.TypeComponentsFactory;
import junit.framework.TestCase;
import org.junit.Assert;

import java.net.URL;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

public class SchemaStructureAppTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjustShort(Logger.getLogger(""));
    }

    public void testAssembleTypeDefinitions() throws Exception {
        for (String xsd : TestApp.Resources.XSD_ARRAY) {
            URL urlInitial = ResourceU.resolve(xsd);
            Assert.assertNotNull(urlInitial);
            logger.info("UrlInitial/" + URLCodec.toExternalForm(urlInitial));
            URL urlCatalog = URLCodec.resolve(urlInitial, ".");
            Assert.assertNotNull(urlInitial);
            logger.finest("UrlCatalog/" + URLCodec.toExternalForm(urlCatalog));
            // resolve schema collection for this initial schema
            SchemaCollectionFactory schemaCollectionFactory = new SchemaCollectionFactory(urlCatalog);
            SchemaCollection schemaCollection = schemaCollectionFactory.create(urlInitial);
            Assert.assertNotNull(schemaCollection);
            // pre-parse of schema collection
            TypeComponentsFactory tcFactory = new TypeComponentsFactory(schemaCollection);
            TypeComponents typeComponents = tcFactory.create();
            // create type definitions for schema set
            TypeDefinitionsFactory tdFactory = new TypeDefinitionsFactory(typeComponents);
            TypeDefinitions typeDefinitions = tdFactory.create();
            trace(typeDefinitions, logger, Level.FINEST);
        }
    }

    public static void trace(TypeDefinitions typeDefinitions, Logger logger, Level level) {
        // inspect model
        for (Map.Entry<String, TypeInstance> entry : typeDefinitions.getElementTypes().entrySet()) {
            logger.log(level, "ELEMENT/" + entry.getKey());
        }
        for (Map.Entry<String, DataType> entry : typeDefinitions.getComplexTypes().entrySet()) {
            logger.log(level, "COMPLEX_TYPE/" + entry.getKey());
        }
        for (Map.Entry<String, DataType> entry : typeDefinitions.getSimpleTypes().entrySet()) {
            logger.log(level, "SIMPLE_TYPE/" + entry.getKey());
        }
    }
}
