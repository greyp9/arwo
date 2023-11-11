package io.github.greyp9.arwo.core.i18n.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppText;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundles;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Locale;
import java.util.ResourceBundle;

public class I18nTest {

    @Test
    public void testCoreBundle() throws Exception {
        final ResourceBundle resourceBundle = new AppText(Locale.getDefault()).getBundleCore();
        Assertions.assertTrue(resourceBundle.keySet().size() > 0);
        final Bundle bundle = new Bundle(resourceBundle);
        Assertions.assertEquals("[{0} row(s)]", bundle.getString(Table.Const.FOOTER_SIZE));
        Assertions.assertEquals("foo", bundle.getString("foo"));
    }

    @Test
    public void testXedBundle() throws Exception {
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final XsdBundle xsdBundle = new XsdBundle(new XsdBundles(xsdTypes, Locale.getDefault()));
        final Xed xed = new Xed(document, xsdTypes, xsdBundle);
        final ResourceBundle resourceBundle = xed.getRootBundle();
        Assertions.assertTrue(resourceBundle.keySet().size() > 0);
        final Bundle bundle = new Bundle(resourceBundle);
        Assertions.assertEquals("Realm", bundle.getString("realm.realmType"));
    }

    @Test
    public void testBundleChain() throws Exception {
        final ResourceBundle resourceBundleCore = new AppText(Locale.getDefault()).getBundleCore();
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final XsdBundle xsdBundle = new XsdBundle(new XsdBundles(xsdTypes, Locale.getDefault()));
        final Xed xed = new Xed(document, xsdTypes, xsdBundle);
        final ResourceBundle resourceBundleDocType = xed.getRootBundle();
        final Bundle bundle = new Bundle(resourceBundleDocType, resourceBundleCore);
        Assertions.assertEquals("[{0} row(s)]", bundle.getString(Table.Const.FOOTER_SIZE));
        Assertions.assertEquals("foo", bundle.getString("foo"));
        Assertions.assertEquals("Realm", bundle.getString("realm.realmType"));
    }
}
