package io.github.greyp9.arwo.core.xed.suite.app.realm.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.XedCursorView;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.XedTableView;
import io.github.greyp9.arwo.core.xed.view.text.PropertyPageTextView;
import io.github.greyp9.arwo.core.xed.view.text.TableTextView;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.source.test.SchemaSourceAppTest;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Collection;
import java.util.Locale;
import java.util.logging.Logger;

public class RealmViewTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testViewI18n() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final TypeInstance typeInstanceRealm = typeDefinitions.getElementTypes().get(qname.toString());
        final TypeInstance typeInstancePrincipals = typeInstanceRealm.getInstance("principals");
        final TypeInstance typeInstancePrincipal = typeInstancePrincipals.getInstance("principal");
        Assert.assertNotNull(typeInstancePrincipal);
        final Document document = new DocumentFactory(typeDefinitions, true).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes, Locale.getDefault());
        final Element elementPrincipal = xed.getXPather().getElement("/realm:realm/realm:principals/realm:principal");
        final XedCursor cursorPrincipal = new XedNav(xed).find(elementPrincipal);
        Assert.assertNotNull(cursorPrincipal);
        // load model
        final XedCursorView view = new XedCursorView(cursorPrincipal);
        final XedPropertyPageView pageView = view.getPageView();
        final Collection<String> itemNames = ViewInstanceU.getItemNames(pageView.getViewInstances());
        logger.finest(itemNames.toString());
        Assert.assertEquals("[user, credential, roles]", itemNames.toString());
        final Collection<String> itemNamesI18n = ViewInstanceU.getItemNamesI18n(
                pageView.getCursor(), pageView.getViewInstances());
        logger.finest(itemNamesI18n.toString());
        Assert.assertEquals("[Name, Password, Roles]", itemNamesI18n.toString());
        // change locale
        final Xed xedDE = new Xed(document, xsdTypes, Locale.GERMAN);
        final XedCursor cursorPrincipalDE = new XedNav(xedDE).find(elementPrincipal);
        final XedCursorView viewDE = new XedCursorView(cursorPrincipalDE);
        final XedPropertyPageView pageViewDE = viewDE.getPageView();
        final Collection<String> itemNamesI18nDE = ViewInstanceU.getItemNamesI18n(
                pageViewDE.getCursor(), pageViewDE.getViewInstances());
        logger.finest(itemNamesI18nDE.toString());
        Assert.assertEquals("[N\u00e4me, P\u00e4ssw\u00f6rd, R\u00f6les]", itemNamesI18nDE.toString());
    }

    public void testViewI18nDE() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final TypeInstance typeInstanceRealm = typeDefinitions.getElementTypes().get(qname.toString());
        final TypeInstance typeInstancePrincipals = typeInstanceRealm.getInstance("principals");
        final TypeInstance typeInstancePrincipal = typeInstancePrincipals.getInstance("principal");
        Assert.assertNotNull(typeInstancePrincipal);
        final Document document = new DocumentFactory(typeDefinitions, true).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes, Locale.GERMAN);
        final Element elementPrincipal = xed.getXPather().getElement("/realm:realm/realm:principals/realm:principal");
        final XedCursor cursorPrincipal = new XedNav(xed).find(elementPrincipal);
        Assert.assertNotNull(cursorPrincipal);
        // load model
        final XedCursorView view = new XedCursorView(cursorPrincipal);
        final XedPropertyPageView pageView = view.getPageView();
        final Collection<String> itemNames = ViewInstanceU.getItemNames(pageView.getViewInstances());
        Assert.assertEquals("[user, credential, roles]", itemNames.toString());
        final Collection<String> itemNamesI18n = ViewInstanceU.getItemNamesI18n(
                pageView.getCursor(), pageView.getViewInstances());
        logger.finest(itemNamesI18n.toString());
        Assert.assertEquals("[N\u00e4me, P\u00e4ssw\u00f6rd, R\u00f6les]", itemNamesI18n.toString());
    }

    public void testCreateView2() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        // navigate
        final XedCursor cursorPrincipalType = new XedNav(xed).find("/ecd28/7256d/8dc37/");
        final Element principals = cursorPrincipalType.getParent().getElement();
        // insert
        final NameTypeValues ntv1 = HttpArguments.toArguments("user=arwo&credential=arwo&roles=*");
        final ValueInstance value1 = ValueInstance.create(cursorPrincipalType.getTypeInstance(), ntv1);
        final Element principal1 = xed.create(cursorPrincipalType.getParent().getElement(), value1);
        Assert.assertNotNull(principal1);
        // verify page
        if (SystemU.isTrue()) {
            final XedCursor cursorPrincipal = new XedNav(xed).find(principal1);
            final XedCursorView cursorView = new XedCursorView(cursorPrincipal);
            final XedPropertyPageView pageView = cursorView.getPageView();
            final String pageText = new PropertyPageTextView(pageView).render();
            logger.finest(SystemU.eol() + pageText);
            Assert.assertEquals(58, pageText.length());
            Assert.assertEquals("74e94e73", CRCU.crc32String(UTF8Codec.toBytes(pageText)));
        }
        // insert
        final NameTypeValues ntv2 = HttpArguments.toArguments("user=arwo2&credential=arwo2&roles=**");
        final ValueInstance value2 = ValueInstance.create(cursorPrincipalType.getTypeInstance(), ntv2);
        final Element principal2 = xed.create(cursorPrincipalType.getParent().getElement(), value2);
        Assert.assertNotNull(principal2);
        // verify page
        if (SystemU.isTrue()) {
            final XedCursor cursorPrincipal = new XedNav(xed).find(principal2);
            final XedCursorView view = new XedCursorView(cursorPrincipal);
            final XedPropertyPageView pageView = view.getPageView();
            final String pageText = new PropertyPageTextView(pageView).render();
            logger.finest(SystemU.eol() + pageText);
            Assert.assertEquals(61, pageText.length());
            Assert.assertEquals("f7dae045", CRCU.crc32String(UTF8Codec.toBytes(pageText)));
        }
        // verify table
        if (SystemU.isTrue()) {
            final XedCursorView view = new XedCursorView(cursorPrincipalType);
            final XedTableView tableView = view.getTableView();
            final String tableText = new TableTextView(tableView).render();
            logger.finest(SystemU.eol() + tableText);
            Assert.assertEquals(85, tableText.length());
            Assert.assertEquals("62784be3", CRCU.crc32String(UTF8Codec.toBytes(tableText)));
        }
        // change locale
        final Xed xedDE = new Xed(document, xsdTypes, Locale.GERMAN);
        // verify page
        if (SystemU.isTrue()) {
            final XedCursor cursorPrincipal = new XedNav(xedDE).find(principal1);
            final XedCursorView view = new XedCursorView(cursorPrincipal);
            final XedPropertyPageView pageView = view.getPageView();
            final String pageText = new PropertyPageTextView(pageView).render();
            logger.finest(SystemU.eol() + pageText);
            Assert.assertEquals(58, pageText.length());
            Assert.assertEquals("cd48b542", CRCU.crc32String(UTF8Codec.toBytes(pageText)));
        }
        // verify table
        if (SystemU.isTrue()) {
            final XedCursor cursorPrincipals = new XedNav(xedDE).find(principals);
            final XedCursor cursorPrincipalTypeDE = new XedNav(xedDE).find("principal", cursorPrincipals);
            final XedCursorView view = new XedCursorView(cursorPrincipalTypeDE);
            final XedTableView tableView = view.getTableView();
            final String tableText = new TableTextView(tableView).render();
            logger.finest(SystemU.eol() + tableText);
            Assert.assertEquals(85, tableText.length());
            Assert.assertEquals("4bdb4168", CRCU.crc32String(UTF8Codec.toBytes(tableText)));
        }
    }
}
