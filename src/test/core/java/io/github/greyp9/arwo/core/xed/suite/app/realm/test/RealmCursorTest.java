package io.github.greyp9.arwo.core.xed.suite.app.realm.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundles;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.XedCursorView;
import io.github.greyp9.arwo.core.xed.view.text.CursorTextView;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Locale;
import java.util.logging.Logger;

public class RealmCursorTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testRealm() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        // generate document
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        logger.finest(DocumentU.toString(document));
        final XsdBundle xsdBundle = new XsdBundle(new XsdBundles(xsdTypes, Locale.getDefault()));
        final Xed xed = new Xed(document, xsdTypes, xsdBundle);
        // insert
        final XedCursor cursorRealm = new XedNav(xed).find(document.getDocumentElement());
        final Element principals = xed.getXPather().getElement("/realm:realm/realm:principals");
        final XedCursor cursorPrincipals = new XedNav(xed).find(principals);
        final XedCursor cursorPrincipalType = new XedNav(xed).find("principal", cursorPrincipals);
        // insert
        final NameTypeValues ntv0 = HttpArguments.toArguments("name=Arwo&salt=Arwo-Salt");
        final ValueInstance value0 = ValueInstance.create(cursorRealm.getTypeInstance(), ntv0);
        final Element realm0 = xed.update(cursorRealm.getElement(), value0);
        Assert.assertNotNull(realm0);
        logger.finest(DocumentU.toString(document));
        // insert
        final NameTypeValues ntv1 = HttpArguments.toArguments("user=arwo1&credential=arwo1&roles=*");
        final ValueInstance value1 = ValueInstance.create(cursorPrincipalType.getTypeInstance(), ntv1);
        final Element principal1 = xed.create(cursorPrincipalType.getParent().getElement(), value1);
        Assert.assertNotNull(principal1);
        // insert
        final NameTypeValues ntv2 = HttpArguments.toArguments("user=arwo2&credential=arwo2&roles=**");
        final ValueInstance value2 = ValueInstance.create(cursorPrincipalType.getTypeInstance(), ntv2);
        final Element principal2 = xed.create(cursorPrincipalType.getParent().getElement(), value2);
        Assert.assertNotNull(principal2);
        logger.finest(DocumentU.toString(document));
        // view at realm
        Assert.assertEquals("/", cursorRealm.getURI());
        final String renderRealm = new CursorTextView(new XedCursorView(cursorRealm)).render();
        logger.finest("Realm\n" + renderRealm);
        Assert.assertEquals("292a3c08", CRCU.crc32String(UTF8Codec.toBytes(renderRealm)));
        // view at principals type
        final XedCursor cursorPrincipalsType = new XedNav(xed).find("principals", cursorRealm);
        Assert.assertEquals("/ecd28/", cursorPrincipalsType.getURI());
        final String renderPrincipalsType = new CursorTextView(new XedCursorView(cursorPrincipalsType)).render();
        logger.finest("PrincipalsType\n" + renderPrincipalsType);
        Assert.assertEquals("8f0f0893", CRCU.crc32String(UTF8Codec.toBytes(renderPrincipalsType)));
        // view at principals
        Assert.assertEquals("/ecd28/7256d/", cursorPrincipals.getURI());
        final String renderPrincipals = new CursorTextView(new XedCursorView(cursorPrincipals)).render();
        logger.finest("Principals\n" + renderPrincipals);
        Assert.assertEquals("5542dd80", CRCU.crc32String(UTF8Codec.toBytes(renderPrincipals)));
        // view at principal type
        Assert.assertEquals("/ecd28/7256d/8dc37/", cursorPrincipalType.getURI());
        final String renderPrincipalType = new CursorTextView(new XedCursorView(cursorPrincipalType)).render();
        logger.finest("PrincipalType\n" + renderPrincipalType);
        Assert.assertEquals("97f3bd05", CRCU.crc32String(UTF8Codec.toBytes(renderPrincipalType)));
        // view at principal1
        final XedCursor cursorPrincipal1 = new XedNav(xed).find(principal1);
        Assert.assertEquals("/ecd28/7256d/8dc37/c3dce/", cursorPrincipal1.getURI());
        final String renderPrincipal1 = new CursorTextView(new XedCursorView(cursorPrincipal1)).render();
        logger.finest("Principal\n" + renderPrincipal1);
        Assert.assertEquals("4f44727f", CRCU.crc32String(UTF8Codec.toBytes(renderPrincipal1)));
        // view at principal2
        final XedCursor cursorPrincipal2 = new XedNav(xed).find(principal2);
        Assert.assertEquals("/ecd28/7256d/8dc37/b0d58/", cursorPrincipal2.getURI());
        final String renderPrincipal2 = new CursorTextView(new XedCursorView(cursorPrincipal2)).render();
        logger.finest("Principal\n" + renderPrincipal2);
        Assert.assertEquals("9b0c5d75", CRCU.crc32String(UTF8Codec.toBytes(renderPrincipal2)));
    }
}
