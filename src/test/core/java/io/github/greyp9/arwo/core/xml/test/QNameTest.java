package io.github.greyp9.arwo.core.xml.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.logging.Logger;

public class QNameTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testQNameColon() {
        final String qnameString = "a:b";
        final QName qname = QNameU.getQNameColon(qnameString);
        Assertions.assertNotNull(qname);
        Assertions.assertEquals("", qname.getNamespaceURI());
        Assertions.assertEquals("b", qname.getLocalPart());
        Assertions.assertEquals("a", qname.getPrefix());
        logger.finest(qname.toString());
    }

    @Test
    public void testQNameToString() {
        final String qnameString = "{a}b";
        final QName qname = QNameU.getQName(qnameString);
        Assertions.assertNotNull(qname);
        Assertions.assertEquals("a", qname.getNamespaceURI());
        Assertions.assertEquals("b", qname.getLocalPart());
        Assertions.assertEquals("", qname.getPrefix());
        logger.finest(qname.toString());
    }

    @Test
    public void testQNameContext() throws Exception {
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = xsdTypes.getQName("realm:realm");
        Assertions.assertEquals(App.Realm.QNAME.getNamespaceURI(), qname.getNamespaceURI());
        Assertions.assertEquals(App.Realm.QNAME.getLocalPart(), qname.getLocalPart());
        Assertions.assertEquals(App.Realm.QNAME.getPrefix(), qname.getPrefix());
        logger.finest(qname.toString());
    }

    @Test
    public void testQNameRender() {
        final QName qname = new QName("urn:a:b", "app");
        Assertions.assertEquals("{urn:a:b}app", qname.toString());
    }
}
