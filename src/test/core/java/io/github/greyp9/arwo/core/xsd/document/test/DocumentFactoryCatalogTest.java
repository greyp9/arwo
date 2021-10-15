package io.github.greyp9.arwo.core.xsd.document.test;

import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.lang.StringFilter;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xsd.atom.SchemaAtom;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollection;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollectionFactory;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitionsFactory;
import io.github.greyp9.arwo.core.xsd.structure.test.SchemaStructureAppTest;
import io.github.greyp9.arwo.core.xsd.type.TypeComponents;
import io.github.greyp9.arwo.core.xsd.type.TypeComponentsFactory;
import io.github.greyp9.arwo.core.xsd.type.test.SchemaTypeAppTest;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.w3c.dom.Document;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

public class DocumentFactoryCatalogTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    @Ignore
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
        int errors = 0;
        // test settings
        String catalogURL = properties.getProperty(String.format("xsd.catalog.%s.url", catalog));
        String xsltURL = properties.getProperty(String.format("xsd.catalog.%s.xslt", catalog));
        Properties includes = PropertiesU.filter(properties, String.format("xsd.catalog.%s.include.*", catalog));
        Properties excludes = PropertiesU.filter(properties, String.format("xsd.catalog.%s.exclude.*", catalog));
        StringFilter stringFilter = new StringFilter(PropertiesU.values(includes), PropertiesU.values(excludes));
        // setup
        URL urlCatalog = URLCodec.toURL(SystemU.resolve(catalogURL));
        URL urlXslt = URLCodec.toURL(SystemU.resolve(xsltURL));
        File folder = URLCodec.toFile(urlCatalog);
        Collection<File> files = new FindInFolderQuery(folder, "*.xsd", true).getFound();
        for (File file : files) {
            // check this resource to see if it should be tested
            URL urlInitial = URLCodec.toURL(file);
            String resource = urlInitial.toExternalForm().replace(urlCatalog.toExternalForm(), "");
            if (stringFilter.matches(resource)) {
                logger.info(file.getPath().replace(folder.getPath(), ""));
                errors += doTestSchema(urlCatalog, urlXslt, urlInitial);
            }
        }
        logger.info(String.format("ERRORS/%d", errors));
    }

    private int doTestSchema(URL urlCatalog, URL urlXslt, URL urlInitial) throws IOException {
        int errors = 0;
        // resolve schema collection for this initial schema
        SchemaCollectionFactory schemaCollectionFactory = new SchemaCollectionFactory(urlCatalog, urlXslt);
        SchemaCollection schemaCollection = schemaCollectionFactory.create(urlInitial);
        String targetNamespace = schemaCollection.getSchemaInitial().getQName().getNamespaceURI();
        for (Map.Entry<String, SchemaAtom> entry : schemaCollection.getSchemas().entrySet()) {
            logger.finest(entry.getKey() + "/" + entry.getValue().getQName());
        }
        // pre-parse of schema collection
        TypeComponentsFactory typeComponentsFactory = new TypeComponentsFactory(schemaCollection);
        TypeComponents typeComponents = typeComponentsFactory.create();
        SchemaTypeAppTest.trace(typeComponents, logger, Level.FINEST);
        // create type definitions for schema set
        TypeDefinitionsFactory typeDefinitionsFactory = new TypeDefinitionsFactory(typeComponents);
        TypeDefinitions typeDefinitions = typeDefinitionsFactory.create();
        SchemaStructureAppTest.trace(typeDefinitions, logger, Level.FINEST);
        boolean[] includeOptionals = { false, true };
        for (boolean includeOptional : includeOptionals) {
            DocumentFactory documentFactory = new DocumentFactory(typeDefinitions, includeOptional);
            for (TypeInstance typeInstance : typeDefinitions.getElementTypes().values()) {
                if (typeInstance.getURI().equals(targetNamespace)) {
                    Document document = documentFactory.generateEmpty(typeInstance.getQName());
                    logger.log(Level.FINEST, DocumentU.toString(document));
                    // validation
                    SchemaAtom schemaAtom = schemaCollection.getSchemaInitial();
                    byte[] xsdInitial = DocumentU.toXml(schemaAtom.getAtom().getElement());
                    errors += DocumentFactoryAppTest.validate(urlInitial, xsdInitial, document, logger);
                }
            }
        }
        return errors;
    }
}
