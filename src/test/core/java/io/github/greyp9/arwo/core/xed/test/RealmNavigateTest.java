package io.github.greyp9.arwo.core.xed.test;

import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
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

public class RealmNavigateTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testNavigateDocument() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        final XPather xpather = xed.getXPather();
        final Element realm = document.getDocumentElement();
        final Element principals = xpather.getElement("/realm:realm/realm:principals");
        // load model
        final TypeInstance typeInstanceRealm = xsdTypes.getElementType(qname.toString());
        final TypeInstance typeInstancePrincipals = typeInstanceRealm.getInstance("principals");
        final TypeInstance typeInstancePrincipal = typeInstancePrincipals.getInstance("principal");
        if (SystemU.isTrue()) {
            // reference to realm (root)
            final XedCursor cursorRoot = new XedNav(xed).getRoot();
            Assert.assertNotNull(cursorRoot);
            Assert.assertEquals(realm, xpather.getElement("/realm:realm"));
            Assert.assertEquals(xed, cursorRoot.getXed());
            Assert.assertEquals(null, cursorRoot.getParent());
            Assert.assertEquals(realm, cursorRoot.getNode());
            Assert.assertEquals(Integer.valueOf(0), cursorRoot.getOrdinal());
            Assert.assertEquals(typeInstanceRealm, cursorRoot.getTypeInstance());
            Assert.assertEquals("/", cursorRoot.getURI());
            // reference to realm (root)
            final XedCursor cursorByNode = new XedNav(xed).find(realm);
            Assert.assertNotNull(cursorByNode);
            Assert.assertEquals(cursorRoot.getXed(), cursorByNode.getXed());
            Assert.assertEquals(cursorRoot.getParent(), cursorByNode.getParent());
            Assert.assertEquals(cursorRoot.getNode(), cursorByNode.getNode());
            Assert.assertEquals(cursorRoot.getOrdinal(), cursorByNode.getOrdinal());
            Assert.assertEquals(cursorRoot.getTypeInstance(), cursorByNode.getTypeInstance());
            Assert.assertEquals(cursorRoot.getURI(), cursorByNode.getURI());
            // reference to realm (root)
            final XedCursor cursorByPath = new XedNav(xed).find("/");
            Assert.assertNotNull(cursorByPath);
            Assert.assertEquals(cursorRoot.getXed(), cursorByPath.getXed());
            Assert.assertEquals(cursorRoot.getParent(), cursorByPath.getParent());
            Assert.assertEquals(cursorRoot.getNode(), cursorByPath.getNode());
            Assert.assertEquals(cursorRoot.getOrdinal(), cursorByPath.getOrdinal());
            Assert.assertEquals(cursorRoot.getTypeInstance(), cursorByPath.getTypeInstance());
            Assert.assertEquals(cursorRoot.getURI(), cursorByPath.getURI());
        }
        if (SystemU.isTrue()) {
            // reference to principalsType
            final XedCursor cursorByPath = new XedNav(xed).find("/ecd28/");
            Assert.assertNotNull(cursorByPath);
            Assert.assertEquals(xed, cursorByPath.getXed());
            Assert.assertNotNull(cursorByPath.getParent());
            Assert.assertNull(cursorByPath.getNode());
            Assert.assertNull(cursorByPath.getOrdinal());
            Assert.assertEquals(typeInstancePrincipals, cursorByPath.getTypeInstance());
            Assert.assertEquals("/ecd28/", cursorByPath.getURI());
        }
        if (SystemU.isTrue()) {
            // reference to principals element
            final XedCursor cursorByNode = new XedNav(xed).find(principals);
            Assert.assertNotNull(cursorByNode);
            Assert.assertEquals(xed, cursorByNode.getXed());
            Assert.assertNotNull(cursorByNode.getParent());
            Assert.assertEquals(principals, cursorByNode.getNode());
            Assert.assertEquals(Integer.valueOf(0), cursorByNode.getOrdinal());
            Assert.assertEquals(typeInstancePrincipals, cursorByNode.getTypeInstance());
            Assert.assertEquals("/ecd28/7256d/", cursorByNode.getURI());
            // reference to principals element
            final XedCursor cursorByPath = new XedNav(xed).find("/ecd28/7256d/");
            Assert.assertNotNull(cursorByPath);
            Assert.assertEquals(xed, cursorByPath.getXed());
            Assert.assertNotNull(cursorByPath.getParent());
            Assert.assertEquals(principals, cursorByPath.getNode());
            Assert.assertEquals(Integer.valueOf(0), cursorByPath.getOrdinal());
            Assert.assertEquals(typeInstancePrincipals, cursorByPath.getTypeInstance());
            Assert.assertEquals("/ecd28/7256d/", cursorByPath.getURI());
        }
        if (SystemU.isTrue()) {
            // reference to principalType
            final XedCursor cursorByPath = new XedNav(xed).find("/ecd28/7256d/8dc37/");
            Assert.assertNotNull(cursorByPath);
            Assert.assertEquals(xed, cursorByPath.getXed());
            Assert.assertNotNull(cursorByPath.getParent());
            Assert.assertNull(cursorByPath.getNode());
            Assert.assertNull(cursorByPath.getOrdinal());
            Assert.assertEquals(typeInstancePrincipal, cursorByPath.getTypeInstance());
            Assert.assertEquals("/ecd28/7256d/8dc37/", cursorByPath.getURI());
        }
        if (SystemU.isTrue()) {
            final Element principal1 = xed.create(principals, ValueInstance.create(typeInstancePrincipal,
                    HttpArguments.toArguments("user=arwo&credential=arwo&roles=*")));
            Assert.assertNotNull(principal1);
            Assert.assertEquals(1, principals.getChildNodes().getLength());
            Assert.assertEquals("arwo", xpather.getText(
                    "/realm:realm/realm:principals/realm:principal[1]/realm:user"));
            // reference to principal element
            final XedCursor cursorByNode = new XedNav(xed).find(principal1);
            Assert.assertNotNull(cursorByNode);
            Assert.assertEquals(xed, cursorByNode.getXed());
            Assert.assertNotNull(cursorByNode.getParent());
            Assert.assertEquals(principal1, cursorByNode.getNode());
            Assert.assertEquals(Integer.valueOf(0), cursorByNode.getOrdinal());
            Assert.assertEquals(typeInstancePrincipal, cursorByNode.getTypeInstance());
            Assert.assertEquals("/ecd28/7256d/8dc37/c3dce/", cursorByNode.getURI());
            // reference to principal element
            final XedCursor cursorByPath = new XedNav(xed).find("/ecd28/7256d/8dc37/c3dce/");
            Assert.assertNotNull(cursorByPath);
            Assert.assertEquals(xed, cursorByPath.getXed());
            Assert.assertNotNull(cursorByPath.getParent());
            Assert.assertEquals(principal1, cursorByPath.getNode());
            Assert.assertEquals(Integer.valueOf(0), cursorByPath.getOrdinal());
            Assert.assertEquals(typeInstancePrincipal, cursorByPath.getTypeInstance());
            Assert.assertEquals("/ecd28/7256d/8dc37/c3dce/", cursorByPath.getURI());
        }
        if (SystemU.isTrue()) {
            final Element principal2 = xed.create(principals, ValueInstance.create(typeInstancePrincipal,
                    HttpArguments.toArguments("user=arwo2&credential=arwo2&roles=**")));
            Assert.assertNotNull(principal2);
            Assert.assertEquals(2, principals.getChildNodes().getLength());
            Assert.assertEquals("arwo2", xpather.getText(
                    "/realm:realm/realm:principals/realm:principal[2]/realm:user"));
            // reference to principal element
            final XedCursor cursorByNode = new XedNav(xed).find(principal2);
            Assert.assertNotNull(cursorByNode);
            Assert.assertEquals(xed, cursorByNode.getXed());
            Assert.assertNotNull(cursorByNode.getParent());
            Assert.assertEquals(principal2, cursorByNode.getNode());
            Assert.assertEquals(Integer.valueOf(1), cursorByNode.getOrdinal());
            Assert.assertEquals(typeInstancePrincipal, cursorByNode.getTypeInstance());
            Assert.assertEquals("/ecd28/7256d/8dc37/b0d58/", cursorByNode.getURI());
            // reference to principal element
            final XedCursor cursorByPath = new XedNav(xed).find("/ecd28/7256d/8dc37/b0d58/");
            Assert.assertNotNull(cursorByPath);
            Assert.assertEquals(xed, cursorByPath.getXed());
            Assert.assertNotNull(cursorByPath.getParent());
            Assert.assertEquals(principal2, cursorByPath.getNode());
            Assert.assertEquals(Integer.valueOf(1), cursorByPath.getOrdinal());
            Assert.assertEquals(typeInstancePrincipal, cursorByPath.getTypeInstance());
            Assert.assertEquals("/ecd28/7256d/8dc37/b0d58/", cursorByPath.getURI());
        }
        logger.finest(DocumentU.toString(document));
        Assert.assertEquals("592e22ef", CRCU.crc32String(DocumentU.toXml(document)));
    }
}
