package io.github.greyp9.arwo.core.xsd.model.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xpath.XPathContext;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;

import javax.xml.namespace.QName;
import java.io.File;
import java.net.URL;
import java.util.logging.Logger;

public class XsdTypesTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testWebAppContext() throws Exception {
        final File fileWebApp25 = new File(SystemU.resolve("~/Downloads/xsd/JavaEE5/web-app_2_5.xsd"));
        if (fileWebApp25.exists()) {
            final URL urlInitial = URLCodec.toURL(fileWebApp25);
            final XsdTypes xsdTypes = new XsdTypes(urlInitial);
            Assert.assertNotNull(xsdTypes);
            // check model
            final XPathContext context = xsdTypes.getContext();
            Assert.assertNotNull(context);
            logger.finest(context.getPrefixToURI().toString());
            Assert.assertEquals(XsdU.NS_URI_XSD, context.getNamespaceURI("xsd"));
            Assert.assertEquals(XsdU.NS_URI_XSD, context.getNamespaceURI("xs"));
            Assert.assertEquals("http://java.sun.com/xml/ns/javaee", context.getNamespaceURI("javaee"));
        }
    }

    @Test
    public void testAppRealmContext() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        Assert.assertNotNull(xsdTypes);
        // check model
        final XPathContext context = xsdTypes.getContext();
        Assert.assertNotNull(context);
        logger.finest(context.getPrefixToURI().toString());
        Assert.assertEquals(XsdU.NS_URI_XSD, context.getNamespaceURI("xsd"));
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
