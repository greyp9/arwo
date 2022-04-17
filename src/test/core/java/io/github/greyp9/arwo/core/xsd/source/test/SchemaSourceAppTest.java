package io.github.greyp9.arwo.core.xsd.source.test;

import io.github.greyp9.arwo.core.app.test.TestApp;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.xsd.atom.SchemaAtom;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollection;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollectionFactory;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.net.URL;
import java.util.Map;
import java.util.logging.Logger;

public class SchemaSourceAppTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjustShort(Logger.getLogger(""));
    }

    @Test
    public void testAssembleSchemaCollection() throws Exception {
        for (String xsd : TestApp.Resources.XSD_ARRAY) {
            URL urlInitial = ResourceU.resolve(xsd);
            Assert.assertNotNull(urlInitial);
            logger.finest("UrlInitial/" + URLCodec.toExternalForm(urlInitial));
            URL urlCatalog = URLCodec.resolve(urlInitial, ".");
            Assert.assertNotNull(urlInitial);
            logger.finest("UrlCatalog/" + URLCodec.toExternalForm(urlCatalog));
            // resolve schema collection for this initial schema
            SchemaCollectionFactory schemaCollectionFactory = new SchemaCollectionFactory(urlCatalog);
            SchemaCollection schemaCollection = schemaCollectionFactory.create(urlInitial);
            Assert.assertNotNull(schemaCollection);
            logger.finest(urlInitial.toExternalForm().replace(urlCatalog.toExternalForm(), ""));
            logger.finest("TargetNamespace/" + schemaCollection.getSchemaInitial().getQName().getNamespaceURI());
            for (Map.Entry<String, SchemaAtom> entry : schemaCollection.getSchemas().entrySet()) {
                logger.finest("Key/" + entry.getKey());
                logger.finest("Value/" + entry.getValue().getAtom().getElement().getTagName());
            }
        }
    }

    @Test
    public void testNoCatalog() throws Exception {
        for (final String xsd : TestApp.Resources.XSD_ARRAY) {
            final URL urlInitial = ResourceU.resolve(xsd);
            Assert.assertNotNull(urlInitial);
            logger.finest("UrlInitial/" + URLCodec.toExternalForm(urlInitial));
            // resolve schema collection for this initial schema
            final SchemaCollectionFactory schemaCollectionFactory = new SchemaCollectionFactory(null);
            final SchemaCollection schemaCollection = schemaCollectionFactory.create(urlInitial);
            Assert.assertNotNull(schemaCollection);
            Assert.assertNotNull(schemaCollection.getSchemas().get(URLCodec.toExternalForm(urlInitial)));
        }
    }
}
