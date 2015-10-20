package io.github.greyp9.arwo.core.xed.test;

import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.source.test.SchemaSourceAppTest;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.logging.Logger;

public class RealmAssembleTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testCreate2() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName nameRealm = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(nameRealm);
        final Xed xed = new Xed(document, xsdTypes);
        // navigate
        final XedCursor cursorPrincipalType = new XedNav(xed).find("/ecd28/7256d/8dc37/");
        // insert
        final NameTypeValues ntv1 = HttpArguments.toArguments("user=arwo&credential=arwo&roles=*");
        final ValueInstance value1 = ValueInstance.create(cursorPrincipalType.getTypeInstance(), ntv1);
        final Element principal1 = xed.create(cursorPrincipalType.getParent().getElement(), value1);
        Assert.assertNotNull(principal1);
        // insert
        final NameTypeValues ntv2 = HttpArguments.toArguments("user=arwo2&credential=arwo2&roles=**");
        final ValueInstance value2 = ValueInstance.create(cursorPrincipalType.getTypeInstance(), ntv2);
        final Element principal2 = xed.create(cursorPrincipalType.getParent().getElement(), value2);
        Assert.assertNotNull(principal2);
        // validate
        logger.finest(DocumentU.toString(document));
        final byte[] xml = DocumentU.toXml(document);
        Assert.assertEquals(305, xml.length);
        Assert.assertEquals("3f3a2b3b", CRCU.crc32String(xml));
    }

    public void testCreateUpdate() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName nameRealm = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(nameRealm);
        final Xed xed = new Xed(document, xsdTypes);
        // navigate
        final XedCursor cursorPrincipalType = new XedNav(xed).find("/ecd28/7256d/8dc37/");
        // insert
        final NameTypeValues ntv1 = HttpArguments.toArguments("user=arwo&credential=arwo&roles=*");
        final ValueInstance value1 = ValueInstance.create(cursorPrincipalType.getTypeInstance(), ntv1);
        final Element principal1 = xed.create(cursorPrincipalType.getParent().getElement(), value1);
        Assert.assertNotNull(principal1);
        // update
        final XedCursor cursor1 = new XedNav(xed).find(principal1);
        final NameTypeValues ntv2 = HttpArguments.toArguments("user=arwo2&credential=arwo2&roles=**");
        final ValueInstance value2 = ValueInstance.create(cursorPrincipalType.getTypeInstance(), ntv2);
        final Element principal2 = xed.update(cursor1.getElement(), value2);
        Assert.assertNotNull(principal2);
        // validate
        logger.finest(DocumentU.toString(document));
        final byte[] xml = DocumentU.toXml(document);
        Assert.assertEquals(215, xml.length);
        Assert.assertEquals("ff619911", CRCU.crc32String(xml));
    }

    public void testCreateDelete() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName nameRealm = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(nameRealm);
        final Xed xed = new Xed(document, xsdTypes);
        // navigate
        final XedCursor cursorPrincipalType = new XedNav(xed).find("/ecd28/7256d/8dc37/");
        // insert
        final NameTypeValues ntv1 = HttpArguments.toArguments("user=arwo&credential=arwo&roles=*");
        final ValueInstance value1 = ValueInstance.create(cursorPrincipalType.getTypeInstance(), ntv1);
        final Element principal1 = xed.create(cursorPrincipalType.getParent().getElement(), value1);
        Assert.assertNotNull(principal1);
        // delete
        final XedCursor cursor1 = new XedNav(xed).find(principal1);
        final Element principalD = xed.delete(cursor1.getElement());
        Assert.assertNotNull(principalD);
        // validate
        logger.finest(DocumentU.toString(document));
        final byte[] xml = DocumentU.toXml(document);
        Assert.assertEquals(109, xml.length);
        Assert.assertEquals("0093040f", CRCU.crc32String(xml));
    }
}
