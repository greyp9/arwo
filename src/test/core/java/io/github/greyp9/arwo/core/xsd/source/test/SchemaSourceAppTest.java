package io.github.greyp9.arwo.core.xsd.source.test;

import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.xsd.atom.SchemaAtom;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollection;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollectionFactory;
import io.github.greyp9.arwo.core.xsd.type.TypeComponents;
import io.github.greyp9.arwo.core.xsd.type.TypeComponentsFactory;
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
            //logger.info(urlInitial.toExternalForm().replace(urlCatalog.toExternalForm(), ""));
            logger.finest("TargetNamespace/" + schemaCollection.getTargetNamespace());
            for (Map.Entry<String, SchemaAtom> entry : schemaCollection.getSchemas().entrySet()) {
                logger.finest("Key/" + entry.getKey());
                logger.finest("Value/" + entry.getValue().getAtom().getElement().getTagName());
            }
        }
    }

    private static class Const {
        private static final String XSD_REALM = "io/github/greyp9/arwo/xsd/realm/realm.xsd";
        private static final String[] XSD_ARRAY = { XSD_REALM };
    }
}