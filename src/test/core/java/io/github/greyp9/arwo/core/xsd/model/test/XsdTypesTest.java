package io.github.greyp9.arwo.core.xsd.model.test;

import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xpath.XPathContext;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.source.test.SchemaSourceAppTest;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import java.io.File;
import java.net.URL;
import java.util.logging.Logger;

public class XsdTypesTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testWebAppContext() throws Exception {
        final File fileWebApp25 = new File(SystemU.resolve("~/Downloads/xsd/JavaEE5/web-app_2_5.xsd"));
        if (fileWebApp25.exists()) {
            final URL urlInitial = URLCodec.toURL(fileWebApp25);
            final XsdTypes xsdTypes = new XsdTypes(urlInitial);
            Assert.assertNotNull(xsdTypes);
            // check model
            final XPathContext context = xsdTypes.getContext();
            Assert.assertNotNull(context);
            logger.info(context.getPrefixToURI().toString());
            Assert.assertEquals(XMLConstants.W3C_XML_SCHEMA_NS_URI, context.getNamespaceURI("xsd"));
            Assert.assertEquals(XMLConstants.W3C_XML_SCHEMA_NS_URI, context.getNamespaceURI("xs"));
            Assert.assertEquals("http://java.sun.com/xml/ns/javaee", context.getNamespaceURI("javaee"));
        }
    }

    public void testAppRealmContext() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        Assert.assertNotNull(xsdTypes);
        // check model
        final XPathContext context = xsdTypes.getContext();
        Assert.assertNotNull(context);
        logger.info(context.getPrefixToURI().toString());
        Assert.assertEquals(XMLConstants.W3C_XML_SCHEMA_NS_URI, context.getNamespaceURI("xsd"));
        Assert.assertEquals("urn:arwo:realm", context.getNamespaceURI("realm"));
        // generate document
        final QName name = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(name);
        // traverse document
        XPather xpather = new XPather(document, xsdTypes.getContext());
        Assert.assertNotNull(xpather.getElement("/realm:realm"));
        Assert.assertNotNull(xpather.getElement("/realm:realm/realm:principals"));
        Assert.assertNull(xpather.getElement("/realm:realm/realm:principals/realm:principal"));
    }
}
