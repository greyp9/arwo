package io.github.greyp9.arwo.core.xsd.source.test;

import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.xsd.atom.SchemaAtom;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollection;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollectionFactory;
import junit.framework.TestCase;
import org.junit.Assert;

import java.net.URL;
import java.util.Map;
import java.util.logging.Logger;

public class SchemaSourceAppTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjustShort(Logger.getLogger(""));
    }

    public void testAssembleSchemaCollection() throws Exception {
        for (String xsd : Const.XSD_ARRAY) {
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
            logger.finest(urlInitial.toExternalForm().replace(urlCatalog.toExternalForm(), ""));
            logger.finest("TargetNamespace/" + schemaCollection.getSchemaInitial().getQName().getNamespaceURI());
            for (Map.Entry<String, SchemaAtom> entry : schemaCollection.getSchemas().entrySet()) {
                logger.finest("Key/" + entry.getKey());
                logger.finest("Value/" + entry.getValue().getAtom().getElement().getTagName());
            }
        }
    }

    public void testNoCatalog() throws Exception {
        for (final String xsd : Const.XSD_ARRAY) {
            final URL urlInitial = ResourceU.resolve(xsd);
            Assert.assertNotNull(urlInitial);
            logger.info("UrlInitial/" + URLCodec.toExternalForm(urlInitial));
            // resolve schema collection for this initial schema
            final SchemaCollectionFactory schemaCollectionFactory = new SchemaCollectionFactory(null);
            final SchemaCollection schemaCollection = schemaCollectionFactory.create(urlInitial);
            Assert.assertNotNull(schemaCollection);
            Assert.assertNotNull(schemaCollection.getSchemas().get(URLCodec.toExternalForm(urlInitial)));
        }
    }

    public static class Const {
        public static final String XSD_REALM = "io/github/greyp9/arwo/xsd/realm/realm.xsd";
        public static final String[] XSD_ARRAY = { XSD_REALM };
    }
}
