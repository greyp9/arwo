package io.github.greyp9.arwo.core.xed.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.extension.XedHash;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.transform.TransformContext;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.logging.Logger;

@SuppressWarnings("checkstyle:magicnumber")
public class RealmAssembleTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testCreate2() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
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
        Assume.assumeTrue(SystemU.javaVersion().startsWith("1.8"));
        Assert.assertEquals(384, xml.length);
        Assert.assertEquals("6e6b144e", CRCU.crc32String(xml));
        // check hashes
        final TypeInstance tiCredential = cursorPrincipalType.getChildInstance("credential");
        final XPather xpather1 = new XPather(principal1, xed.getXPather().getContext());
        final String hash1 = XedHash.getHash(tiCredential, "arwo", new TransformContext(null, xpather1));
        Assert.assertEquals("1/LNHf3yHLrcgbvDIN9OBPTppq5rSWUa7kfTORfChr8=", hash1);
        final XPather xpather2 = new XPather(principal2, xed.getXPather().getContext());
        final String hash2 = XedHash.getHash(tiCredential, "arwo2", new TransformContext(null, xpather2));
        Assert.assertEquals("i1Otk1K9L0SBdbqYUkno5UoEAjXd9iBKLPhCxPpy2o0=", hash2);
    }

    @Test
    public void testCreateUpdate() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        // navigate
        final XedCursor cursorPrincipalType = new XedNav(xed).find("/ecd28/7256d/8dc37/");
        // insert
        final NameTypeValues ntv1 = HttpArguments.toArguments("user=arwo&credential=arwo&roles=*");
        final ValueInstance value1 = ValueInstance.create(cursorPrincipalType.getTypeInstance(), ntv1);
        final Element principal1 = xed.create(cursorPrincipalType.getParent().getElement(), value1);
        Assert.assertNotNull(principal1);
        // check hash
        logger.finest(DocumentU.toString(document));
        final TypeInstance tiCredential = cursorPrincipalType.getChildInstance("credential");
        final XPather xpather1 = new XPather(principal1, xed.getXPather().getContext());
        final String hash1 = XedHash.getHash(tiCredential, "arwo", new TransformContext(null, xpather1));
        Assert.assertEquals("1/LNHf3yHLrcgbvDIN9OBPTppq5rSWUa7kfTORfChr8=", hash1);
        // update
        final XedCursor cursor1 = new XedNav(xed).find(principal1);
        final NameTypeValues ntv2 = HttpArguments.toArguments("user=arwo2&credential=arwo2&roles=**");
        final ValueInstance value2 = ValueInstance.create(cursorPrincipalType.getTypeInstance(), ntv2);
        final Element principal2 = xed.update(cursor1.getElement(), value2);
        Assert.assertNotNull(principal2);
        // validate
        logger.finest(DocumentU.toString(document));
        final byte[] xml = DocumentU.toXml(document);
        Assume.assumeTrue(SystemU.javaVersion().startsWith("1.8"));
        Assert.assertEquals(254, xml.length);
        Assert.assertEquals("7487e72a", CRCU.crc32String(xml));
        // check hash
        final XPather xpather2 = new XPather(principal2, xed.getXPather().getContext());
        final String hash2 = XedHash.getHash(tiCredential, "arwo2", new TransformContext(null, xpather2));
        Assert.assertEquals("i1Otk1K9L0SBdbqYUkno5UoEAjXd9iBKLPhCxPpy2o0=", hash2);
    }

    @Test
    public void testCreateDelete() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
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
        Assume.assumeTrue(SystemU.javaVersion().startsWith("1.8"));
        Assert.assertEquals(109, xml.length);
        Assert.assertEquals("0093040f", CRCU.crc32String(xml));
    }
}
