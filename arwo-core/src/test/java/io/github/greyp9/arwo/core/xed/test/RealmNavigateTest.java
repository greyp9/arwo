package io.github.greyp9.arwo.core.xed.test;

import io.github.greyp9.arwo.core.app.App;
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
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.logging.Logger;

public class RealmNavigateTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testNavigateDocument() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
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
            Assertions.assertNotNull(cursorRoot);
            Assertions.assertEquals(realm, xpather.getElement("/realm:realm"));
            Assertions.assertEquals(xed, cursorRoot.getXed());
            Assertions.assertEquals(null, cursorRoot.getParent());
            Assertions.assertEquals(realm, cursorRoot.getNode());
            Assertions.assertEquals(Integer.valueOf(0), cursorRoot.getOrdinal());
            Assertions.assertEquals(typeInstanceRealm, cursorRoot.getTypeInstance());
            Assertions.assertEquals("/", cursorRoot.getURI());
            // reference to realm (root)
            final XedCursor cursorByNode = new XedNav(xed).find(realm);
            Assertions.assertNotNull(cursorByNode);
            Assertions.assertEquals(cursorRoot.getXed(), cursorByNode.getXed());
            Assertions.assertEquals(cursorRoot.getParent(), cursorByNode.getParent());
            Assertions.assertEquals(cursorRoot.getNode(), cursorByNode.getNode());
            Assertions.assertEquals(cursorRoot.getOrdinal(), cursorByNode.getOrdinal());
            Assertions.assertEquals(cursorRoot.getTypeInstance(), cursorByNode.getTypeInstance());
            Assertions.assertEquals(cursorRoot.getURI(), cursorByNode.getURI());
            // reference to realm (root)
            final XedCursor cursorByPath = new XedNav(xed).find("/");
            Assertions.assertNotNull(cursorByPath);
            Assertions.assertEquals(cursorRoot.getXed(), cursorByPath.getXed());
            Assertions.assertEquals(cursorRoot.getParent(), cursorByPath.getParent());
            Assertions.assertEquals(cursorRoot.getNode(), cursorByPath.getNode());
            Assertions.assertEquals(cursorRoot.getOrdinal(), cursorByPath.getOrdinal());
            Assertions.assertEquals(cursorRoot.getTypeInstance(), cursorByPath.getTypeInstance());
            Assertions.assertEquals(cursorRoot.getURI(), cursorByPath.getURI());
        }
        if (SystemU.isTrue()) {
            // reference to principalsType
            final XedCursor cursorByPath = new XedNav(xed).find("/ecd28/");
            Assertions.assertNotNull(cursorByPath);
            Assertions.assertEquals(xed, cursorByPath.getXed());
            Assertions.assertNotNull(cursorByPath.getParent());
            Assertions.assertNull(cursorByPath.getNode());
            Assertions.assertNull(cursorByPath.getOrdinal());
            Assertions.assertEquals(typeInstancePrincipals, cursorByPath.getTypeInstance());
            Assertions.assertEquals("/ecd28/", cursorByPath.getURI());
        }
        if (SystemU.isTrue()) {
            // reference to principals element
            final XedCursor cursorByNode = new XedNav(xed).find(principals);
            Assertions.assertNotNull(cursorByNode);
            Assertions.assertEquals(xed, cursorByNode.getXed());
            Assertions.assertNotNull(cursorByNode.getParent());
            Assertions.assertEquals(principals, cursorByNode.getNode());
            Assertions.assertEquals(Integer.valueOf(0), cursorByNode.getOrdinal());
            Assertions.assertEquals(typeInstancePrincipals, cursorByNode.getTypeInstance());
            Assertions.assertEquals("/ecd28/7256d/", cursorByNode.getURI());
            // reference to principals element
            final XedCursor cursorByPath = new XedNav(xed).find("/ecd28/7256d/");
            Assertions.assertNotNull(cursorByPath);
            Assertions.assertEquals(xed, cursorByPath.getXed());
            Assertions.assertNotNull(cursorByPath.getParent());
            Assertions.assertEquals(principals, cursorByPath.getNode());
            Assertions.assertEquals(Integer.valueOf(0), cursorByPath.getOrdinal());
            Assertions.assertEquals(typeInstancePrincipals, cursorByPath.getTypeInstance());
            Assertions.assertEquals("/ecd28/7256d/", cursorByPath.getURI());
        }
        if (SystemU.isTrue()) {
            // reference to principalType
            final XedCursor cursorByPath = new XedNav(xed).find("/ecd28/7256d/8dc37/");
            Assertions.assertNotNull(cursorByPath);
            Assertions.assertEquals(xed, cursorByPath.getXed());
            Assertions.assertNotNull(cursorByPath.getParent());
            Assertions.assertNull(cursorByPath.getNode());
            Assertions.assertNull(cursorByPath.getOrdinal());
            Assertions.assertEquals(typeInstancePrincipal, cursorByPath.getTypeInstance());
            Assertions.assertEquals("/ecd28/7256d/8dc37/", cursorByPath.getURI());
        }
        if (SystemU.isTrue()) {
            final Element principal1 = xed.create(principals, ValueInstance.create(typeInstancePrincipal,
                    HttpArguments.toArguments("user=arwo&credential=arwo&roles=*")));
            Assertions.assertNotNull(principal1);
            Assertions.assertEquals(1, principals.getChildNodes().getLength());
            Assertions.assertEquals("arwo", xpather.getText(
                    "/realm:realm/realm:principals/realm:principal[1]/realm:user"));
            // reference to principal element
            final XedCursor cursorByNode = new XedNav(xed).find(principal1);
            Assertions.assertNotNull(cursorByNode);
            Assertions.assertEquals(xed, cursorByNode.getXed());
            Assertions.assertNotNull(cursorByNode.getParent());
            Assertions.assertEquals(principal1, cursorByNode.getNode());
            Assertions.assertEquals(Integer.valueOf(0), cursorByNode.getOrdinal());
            Assertions.assertEquals(typeInstancePrincipal, cursorByNode.getTypeInstance());
            Assertions.assertEquals("/ecd28/7256d/8dc37/c3dce/", cursorByNode.getURI());
            // reference to principal element
            final XedCursor cursorByPath = new XedNav(xed).find("/ecd28/7256d/8dc37/c3dce/");
            Assertions.assertNotNull(cursorByPath);
            Assertions.assertEquals(xed, cursorByPath.getXed());
            Assertions.assertNotNull(cursorByPath.getParent());
            Assertions.assertEquals(principal1, cursorByPath.getNode());
            Assertions.assertEquals(Integer.valueOf(0), cursorByPath.getOrdinal());
            Assertions.assertEquals(typeInstancePrincipal, cursorByPath.getTypeInstance());
            Assertions.assertEquals("/ecd28/7256d/8dc37/c3dce/", cursorByPath.getURI());
        }
        if (SystemU.isTrue()) {
            final Element principal2 = xed.create(principals, ValueInstance.create(typeInstancePrincipal,
                    HttpArguments.toArguments("user=arwo2&credential=arwo2&roles=**")));
            Assertions.assertNotNull(principal2);
            Assertions.assertEquals(2, principals.getChildNodes().getLength());
            Assertions.assertEquals("arwo2", xpather.getText(
                    "/realm:realm/realm:principals/realm:principal[2]/realm:user"));
            // reference to principal element
            final XedCursor cursorByNode = new XedNav(xed).find(principal2);
            Assertions.assertNotNull(cursorByNode);
            Assertions.assertEquals(xed, cursorByNode.getXed());
            Assertions.assertNotNull(cursorByNode.getParent());
            Assertions.assertEquals(principal2, cursorByNode.getNode());
            Assertions.assertEquals(Integer.valueOf(1), cursorByNode.getOrdinal());
            Assertions.assertEquals(typeInstancePrincipal, cursorByNode.getTypeInstance());
            Assertions.assertEquals("/ecd28/7256d/8dc37/b0d58/", cursorByNode.getURI());
            // reference to principal element
            final XedCursor cursorByPath = new XedNav(xed).find("/ecd28/7256d/8dc37/b0d58/");
            Assertions.assertNotNull(cursorByPath);
            Assertions.assertEquals(xed, cursorByPath.getXed());
            Assertions.assertNotNull(cursorByPath.getParent());
            Assertions.assertEquals(principal2, cursorByPath.getNode());
            Assertions.assertEquals(Integer.valueOf(1), cursorByPath.getOrdinal());
            Assertions.assertEquals(typeInstancePrincipal, cursorByPath.getTypeInstance());
            Assertions.assertEquals("/ecd28/7256d/8dc37/b0d58/", cursorByPath.getURI());
        }
        logger.finest(DocumentU.toString(document));
        if (SystemU.javaVersion().startsWith("1.8")) {
            Assertions.assertEquals("6e6b144e", CRCU.crc32String(DocumentU.toXml(document)));
        }
    }
}
