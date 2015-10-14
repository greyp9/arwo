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
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

public class DocumentFactoryCatalogTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
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
        int errors = 0;
        // test settings
        String catalogURL = properties.getProperty(String.format("xsd.catalog.%s.url", catalog));
        String xsltURL = properties.getProperty(String.format("xsd.catalog.%s.xslt", catalog));
        Properties includes = PropertiesU.filter(properties, String.format("xsd.catalog.%s.include.*", catalog));
        Properties excludes = PropertiesU.filter(properties, String.format("xsd.catalog.%s.exclude.*", catalog));
        StringFilter stringFilter = new StringFilter(PropertiesU.values(includes), PropertiesU.values(excludes));
        // setup
        URL urlCatalog = new URL(catalogURL);
        URL urlXslt = URLCodec.toURL(xsltURL);
        File folder = URLCodec.toFile(urlCatalog);
        Collection<File> files = new FindInFolderQuery(folder, "*.xsd", true).getFound();
        for (File file : files) {
            // check this resource to see if it should be tested
            URL urlInitial = URLCodec.toURL(file);
            String resource = urlInitial.toExternalForm().replace(urlCatalog.toExternalForm(), "");
            if (stringFilter.matches(resource)) {
                logger.info(file.getPath().replace(folder.getPath(), ""));
                errors += doTestSchema(errors, urlCatalog, urlXslt, urlInitial);
            }
        }
        logger.info(String.format("ERRORS/%d", errors));
    }

    private int doTestSchema(int errors, URL urlCatalog, URL urlXslt, URL urlInitial) throws IOException {
        // resolve schema collection for this initial schema
        SchemaCollectionFactory schemaCollectionFactory = new SchemaCollectionFactory(urlCatalog, urlXslt);
        SchemaCollection schemaCollection = schemaCollectionFactory.create(urlInitial);
        String targetNamespace = schemaCollection.getTargetNamespace();
        for (Map.Entry<String, SchemaAtom> entry : schemaCollection.getSchemas().entrySet()) {
            logger.finest(entry.getKey() + "/" + entry.getValue());
        }
        // pre-parse of schema collection
        TypeComponentsFactory tcFactory = new TypeComponentsFactory(schemaCollection);
        TypeComponents typeComponents = tcFactory.create();
        SchemaTypeAppTest.trace(typeComponents, logger, Level.FINEST);
        // create type definitions for schema set
        TypeDefinitionsFactory tdFactory = new TypeDefinitionsFactory(typeComponents);
        TypeDefinitions typeDefinitions = tdFactory.create();
        SchemaStructureAppTest.trace(typeDefinitions, logger, Level.FINEST);
        boolean[] includeOptionals = { false, true };
        for (boolean includeOptional : includeOptionals) {
            DocumentFactory documentFactory = new DocumentFactory(typeDefinitions, includeOptional);
            for (TypeInstance typeInstance : typeDefinitions.getElementTypes().values()) {
                if (typeInstance.getURI().equals(targetNamespace)) {
                    Document document = documentFactory.generateEmpty(typeInstance.getQName());
                    logger.log(Level.FINEST, DocumentU.toString(document));
                    // validation
                    SchemaAtom schemaAtom = schemaCollection.getSchemas().get(targetNamespace);
                    byte[] xsdInitial = DocumentU.toXml(schemaAtom.getAtom().getElement());
                    errors += DocumentFactoryAppTest.validate(urlInitial, xsdInitial, document, logger);
                }
            }
        }
        return errors;
    }
}
