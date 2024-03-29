package io.github.greyp9.arwo.core.xed.suite.app.realm.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundles;
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
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Collection;
import java.util.Locale;
import java.util.logging.Logger;

public class RealmViewTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testViewI18n() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final TypeInstance typeInstanceRealm = typeDefinitions.getElementTypes().get(qname.toString());
        final TypeInstance typeInstancePrincipals = typeInstanceRealm.getInstance("principals");
        final TypeInstance typeInstancePrincipal = typeInstancePrincipals.getInstance("principal");
        Assertions.assertNotNull(typeInstancePrincipal);
        final Document document = new DocumentFactory(typeDefinitions, true).generateEmpty(qname);
        final XsdBundle xsdBundle = new XsdBundle(new XsdBundles(xsdTypes, Locale.getDefault()));
        final XsdBundle xsdBundleDE = new XsdBundle(new XsdBundles(xsdTypes, Locale.GERMAN));
        final boolean isAvailableI18n =
                !xsdBundleDE.getLabel(typeInstanceRealm).equals(xsdBundle.getLabel(typeInstanceRealm));
        final Xed xed = new Xed(document, xsdTypes, xsdBundle);
        final Element elementPrincipal = xed.getXPather().getElement("/realm:realm/realm:principals/realm:principal");
        final XedCursor cursorPrincipal = new XedNav(xed).find(elementPrincipal);
        Assertions.assertNotNull(cursorPrincipal);
        // load model
        final XedCursorView view = new XedCursorView(cursorPrincipal);
        final XedPropertyPageView pageView = view.getPageView();
        final Collection<String> itemNames = ViewInstanceU.getItemNames(pageView.getViewInstances());
        logger.finest(itemNames.toString());
        Assertions.assertEquals("[user, credential, roles]", itemNames.toString());
        final Collection<String> itemNamesI18n = ViewInstanceU.getItemNamesI18n(
                pageView.getCursor(), pageView.getViewInstances());
        logger.finest(itemNamesI18n.toString());
        Assertions.assertEquals("[Name, Password, Roles]", itemNamesI18n.toString());
        // change locale
        final Xed xedDE = new Xed(document, xsdTypes, xsdBundleDE);
        final XedCursor cursorPrincipalDE = new XedNav(xedDE).find(elementPrincipal);
        final XedCursorView viewDE = new XedCursorView(cursorPrincipalDE);
        final XedPropertyPageView pageViewDE = viewDE.getPageView();
        final Collection<String> itemNamesI18nDE = ViewInstanceU.getItemNamesI18n(
                pageViewDE.getCursor(), pageViewDE.getViewInstances());
        logger.finest(itemNamesI18nDE.toString());
        // pseudo i18n bundles are assembled by Maven antrun plugin, which runs on successful build
        // if i18n bundles are available, expect i18n text; otherwise, expect default locale text
        // on "mvn clean package", i18n bundles will not present; on subsequent "mvn package" they will be present
        final String expected = isAvailableI18n
                ? "[N\u00e4me, P\u00e4ssw\u00f6rd, R\u00f6les]" : "[Name, Password, Roles]";
        Assertions.assertEquals(expected, itemNamesI18nDE.toString());
    }

    @Test
    public void testViewI18nDE() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final TypeInstance typeInstanceRealm = typeDefinitions.getElementTypes().get(qname.toString());
        final TypeInstance typeInstancePrincipals = typeInstanceRealm.getInstance("principals");
        final TypeInstance typeInstancePrincipal = typeInstancePrincipals.getInstance("principal");
        Assertions.assertNotNull(typeInstancePrincipal);
        final Document document = new DocumentFactory(typeDefinitions, true).generateEmpty(qname);
        final XsdBundle xsdBundleDF = new XsdBundle(new XsdBundles(xsdTypes, Locale.getDefault()));
        final XsdBundle xsdBundleDE = new XsdBundle(new XsdBundles(xsdTypes, Locale.GERMAN));
        final boolean isAvailableI18n =
                !xsdBundleDE.getLabel(typeInstanceRealm).equals(xsdBundleDF.getLabel(typeInstanceRealm));

        final Xed xed = new Xed(document, xsdTypes, xsdBundleDE);
        final Element elementPrincipal = xed.getXPather().getElement("/realm:realm/realm:principals/realm:principal");
        final XedCursor cursorPrincipal = new XedNav(xed).find(elementPrincipal);
        Assertions.assertNotNull(cursorPrincipal);
        // load model
        final XedCursorView view = new XedCursorView(cursorPrincipal);
        final XedPropertyPageView pageView = view.getPageView();
        final Collection<String> itemNames = ViewInstanceU.getItemNames(pageView.getViewInstances());
        Assertions.assertEquals("[user, credential, roles]", itemNames.toString());
        final Collection<String> itemNamesI18n = ViewInstanceU.getItemNamesI18n(
                pageView.getCursor(), pageView.getViewInstances());
        logger.finest(itemNamesI18n.toString());
        // pseudo i18n bundles are assembled by Maven antrun plugin, which runs on successful build
        // if i18n bundles are available, expect i18n text; otherwise, expect default locale text
        final String expected = isAvailableI18n
                ? "[N\u00e4me, P\u00e4ssw\u00f6rd, R\u00f6les]" : "[Name, Password, Roles]";
        Assertions.assertEquals(expected, itemNamesI18n.toString());
    }

    @SuppressWarnings("checkstyle:magicnumber")
    @Test
    public void testCreateView2() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        final XsdBundle xsdBundle = new XsdBundle(new XsdBundles(xsdTypes, Locale.getDefault()));
        final XsdBundle xsdBundleDE = new XsdBundle(new XsdBundles(xsdTypes, Locale.GERMAN));
        final Xed xedUI = new Xed(xed.getDocument(), xed.getXsdTypes(), xsdBundle);
        // navigate
        final XedCursor cursorPrincipalType = new XedNav(xedUI).find("/ecd28/7256d/8dc37/");
        final Element principals = cursorPrincipalType.getParent().getElement();
        // insert
        final NameTypeValues ntv1 = HttpArguments.toArguments("user=arwo&credential=arwo&roles=*");
        final ValueInstance value1 = ValueInstance.create(cursorPrincipalType.getTypeInstance(), ntv1);
        final Element principal1 = xedUI.create(cursorPrincipalType.getParent().getElement(), value1);
        Assertions.assertNotNull(principal1);
        // verify page
        if (SystemU.isTrue()) {
            final XedCursor cursorPrincipal = new XedNav(xedUI).find(principal1);
            final XedCursorView cursorView = new XedCursorView(cursorPrincipal);
            final XedPropertyPageView pageView = cursorView.getPageView();
            final String pageText = new PropertyPageTextView(pageView).render();
            logger.finest(SystemU.eol() + pageText);
            Assertions.assertEquals(178, pageText.length());
            Assertions.assertEquals("b7ca26b2", CRCU.crc32String(UTF8Codec.toBytes(pageText)));
        }
        // insert
        final NameTypeValues ntv2 = HttpArguments.toArguments("user=arwo2&credential=arwo2&roles=**");
        final ValueInstance value2 = ValueInstance.create(cursorPrincipalType.getTypeInstance(), ntv2);
        final Element principal2 = xedUI.create(cursorPrincipalType.getParent().getElement(), value2);
        Assertions.assertNotNull(principal2);
        // verify page
        if (SystemU.isTrue()) {
            final XedCursor cursorPrincipal = new XedNav(xedUI).find(principal2);
            final XedCursorView view = new XedCursorView(cursorPrincipal);
            final XedPropertyPageView pageView = view.getPageView();
            final String pageText = new PropertyPageTextView(pageView).render();
            logger.finest(SystemU.eol() + pageText);
            Assertions.assertEquals(178, pageText.length());
            Assertions.assertEquals("d707be74", CRCU.crc32String(UTF8Codec.toBytes(pageText)));
        }
        // verify table
        if (SystemU.isTrue()) {
            final XedCursorView view = new XedCursorView(cursorPrincipalType);
            final XedTableView tableView = view.getTableView();
            final String tableText = new TableTextView(tableView).render();
            logger.finest(SystemU.eol() + tableText);
            Assertions.assertEquals(193, tableText.length());
            Assertions.assertEquals("d3fa20e5", CRCU.crc32String(UTF8Codec.toBytes(tableText)));
        }
        // change locale
        final Xed xedDE = new Xed(document, xsdTypes, xsdBundleDE);
        // verify page
        if (SystemU.isTrue()) {
            final XedCursor cursorPrincipal = new XedNav(xedDE).find(principal1);
            final XedCursorView view = new XedCursorView(cursorPrincipal);
            final XedPropertyPageView pageView = view.getPageView();
            final String pageText = new PropertyPageTextView(pageView).render();
            logger.finest(SystemU.eol() + pageText);
            Assertions.assertEquals(178, pageText.length());
            // pseudo i18n bundles are assembled by Maven antrun plugin, which runs on successful build
            // if i18n bundles are available, expect RU text; otherwise, expect default locale text
            final TypeInstance typeInstance = cursorPrincipal.getTypeInstance();
            final boolean isAvailableI18n =
                    !xsdBundleDE.getLabel(typeInstance).equals(xsdBundle.getLabel(typeInstance));
            final String expected = isAvailableI18n ? "5b3c3809" : "b7ca26b2";
            Assertions.assertEquals(expected, CRCU.crc32String(UTF8Codec.toBytes(pageText)));
        }
        // verify table
        if (SystemU.isTrue()) {
            final XedCursor cursorPrincipals = new XedNav(xedDE).find(principals);
            Assertions.assertNotNull(cursorPrincipals);
            final XedCursor cursorPrincipalTypeDE = new XedNav(xedDE).find("principal", cursorPrincipals);
            final XedCursorView view = new XedCursorView(cursorPrincipalTypeDE);
            final XedTableView tableView = view.getTableView();
            final String tableText = new TableTextView(tableView).render();
            logger.finest(SystemU.eol() + tableText);
            Assertions.assertEquals(193, tableText.length());
            // pseudo i18n bundles are assembled by Maven antrun plugin, which runs on successful build
            // if i18n bundles are available, expect RU text; otherwise, expect default locale text
            final TypeInstance typeInstance = cursorPrincipals.getTypeInstance();
            final boolean isAvailableI18n =
                    !xsdBundleDE.getLabel(typeInstance).equals(xsdBundle.getLabel(typeInstance));
            final String expected = isAvailableI18n ? "61ff36bb" : "d3fa20e5";
            Assertions.assertEquals(expected, CRCU.crc32String(UTF8Codec.toBytes(tableText)));
        }
    }
}
