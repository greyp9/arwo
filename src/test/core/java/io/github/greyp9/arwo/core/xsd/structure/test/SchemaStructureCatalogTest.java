package io.github.greyp9.arwo.core.xsd.structure.test;

import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollection;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollectionFactory;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitionsFactory;
import io.github.greyp9.arwo.core.xsd.type.TypeComponents;
import io.github.greyp9.arwo.core.xsd.type.TypeComponentsFactory;
import junit.framework.TestCase;
import org.junit.Assert;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

public class SchemaStructureCatalogTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjustShort(Logger.getLogger(""));
    }

    public void testSchemaCatalogs() throws Exception {
        File fileProperties = new File(SystemU.userHome(), ".arwo/test.properties.xml");
        Assert.assertTrue(fileProperties.exists());
        Properties properties = PropertiesU.loadFromXml(fileProperties.toURI().toURL());
        logger.finest("" + properties.size());
        Assert.assertTrue(properties.size() > 0);
        String catalogList = properties.getProperty("xsd.catalog");
        Assert.assertNotNull(catalogList);
        String[] catalogs = catalogList.split(",");
        for (String catalog : catalogs) {
            if (catalog.length() > 0) {
                doTestSchemaCatalog(catalog, properties);
            }
        }
    }

    private void doTestSchemaCatalog(String catalog, Properties properties) throws IOException {
        String catalogURL = properties.getProperty(String.format("xsd.catalog.%s.url", catalog));
        String xsltURL = properties.getProperty(String.format("xsd.catalog.%s.xslt", catalog));
        URL urlCatalog = new URL(catalogURL);
        URL urlXslt = URLCodec.toURL(xsltURL);
        File folder = URLCodec.toFile(urlCatalog);
        Collection<File> files = new FindInFolderQuery(folder, "*.xsd", true).getFound();
        for (File file : files) {
            logger.info(file.getPath().replace(folder.getPath(), ""));
            // resolve schema collection for this initial schema
            SchemaCollectionFactory schemaCollectionFactory = new SchemaCollectionFactory(urlCatalog, urlXslt);
            SchemaCollection schemaCollection = schemaCollectionFactory.create(URLCodec.toURL(file));
            Assert.assertNotNull(schemaCollection);
            // pre-parse of schema collection
            TypeComponentsFactory tcFactory = new TypeComponentsFactory(schemaCollection);
            TypeComponents typeComponents = tcFactory.create();
            // create type definitions for schema set
            TypeDefinitionsFactory tdFactory = new TypeDefinitionsFactory(typeComponents);
            TypeDefinitions typeDefinitions = tdFactory.create();
            SchemaStructureAppTest.trace(typeDefinitions, logger, Level.INFO);
        }
    }
}
