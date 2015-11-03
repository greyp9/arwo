package io.github.greyp9.arwo.core.i18n.test;

import io.github.greyp9.arwo.core.app.AppText;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.source.test.SchemaSourceAppTest;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Locale;
import java.util.ResourceBundle;

public class I18nTest extends TestCase {

    public void testCoreBundle() throws Exception {
        final ResourceBundle resourceBundle = new AppText(Locale.getDefault()).getBundleCore();
        Assert.assertTrue(resourceBundle.keySet().size() > 0);
        final Bundle bundle = new Bundle(resourceBundle);
        Assert.assertEquals("[{0} row(s)]", bundle.getString(Table.Const.FOOTER_SIZE));
        Assert.assertEquals("foo", bundle.getString("foo"));
    }

    public void testXedBundle() throws Exception {
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes, Locale.getDefault());
        final ResourceBundle resourceBundle = xed.getRootBundle();
        Assert.assertTrue(resourceBundle.keySet().size() > 0);
        final Bundle bundle = new Bundle(resourceBundle);
        Assert.assertEquals("Realm", bundle.getString("realm.realmType"));
    }

    public void testBundleChain() throws Exception {
        final ResourceBundle resourceBundleCore = new AppText(Locale.getDefault()).getBundleCore();
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes, Locale.getDefault());
        final ResourceBundle resourceBundleDocType = xed.getRootBundle();
        final Bundle bundle = new Bundle(resourceBundleDocType, resourceBundleCore);
        Assert.assertEquals("[{0} row(s)]", bundle.getString(Table.Const.FOOTER_SIZE));
        Assert.assertEquals("foo", bundle.getString("foo"));
        Assert.assertEquals("Realm", bundle.getString("realm.realmType"));
    }
}
