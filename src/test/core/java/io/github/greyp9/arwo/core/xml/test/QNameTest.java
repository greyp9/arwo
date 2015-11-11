package io.github.greyp9.arwo.core.xml.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import junit.framework.TestCase;
import org.junit.Assert;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.logging.Logger;

public class QNameTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public void testQNameColon() throws Exception {
        final String qnameString = "a:b";
        final QName qname = QNameU.getQNameColon(qnameString);
        Assert.assertNotNull(qname);
        Assert.assertEquals("", qname.getNamespaceURI());
        Assert.assertEquals("b", qname.getLocalPart());
        Assert.assertEquals("a", qname.getPrefix());
        logger.finest(qname.toString());
    }

    public void testQNameToString() throws Exception {
        final String qnameString = "{a}b";
        final QName qname = QNameU.getQName(qnameString);
        Assert.assertNotNull(qname);
        Assert.assertEquals("a", qname.getNamespaceURI());
        Assert.assertEquals("b", qname.getLocalPart());
        Assert.assertEquals("", qname.getPrefix());
        logger.finest(qname.toString());
    }

    public void testQNameContext() throws Exception {
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = xsdTypes.getQName("realm:realm");
        Assert.assertEquals(App.Realm.QNAME.getNamespaceURI(), qname.getNamespaceURI());
        Assert.assertEquals(App.Realm.QNAME.getLocalPart(), qname.getLocalPart());
        Assert.assertEquals(App.Realm.QNAME.getPrefix(), qname.getPrefix());
        logger.finest(qname.toString());
    }
}
