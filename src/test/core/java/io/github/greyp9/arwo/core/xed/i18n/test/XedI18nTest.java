package io.github.greyp9.arwo.core.xed.i18n.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundles;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Document;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Locale;
import java.util.ResourceBundle;

public class XedI18nTest {

    @Test
    public void testRealm() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(typeDefinitions, false).generateEmpty(qname);
        final XsdBundle xsdBundleNL = new XsdBundle(new XsdBundles(xsdTypes, null));
        final Xed xedNL = new Xed(document, xsdTypes, xsdBundleNL);
        // check top-level type
        final TypeInstance typeInstance = typeDefinitions.getElementTypes().get(qname.toString());
        Assert.assertNotNull(typeInstance);
        // i18n test values
        final String key = "realm.realmType";
        final String valueDF = "Realm";
        final String valueJA = "\u5c3a\u30e8\u4e39\u3057m";
        // check i18n (should be default, as no locale specified)
        final ResourceBundle bundleNull = xedNL.getBundle(typeInstance.getURI());
        Assert.assertNull(bundleNull);
        // update locale, check i18n
        final XsdBundle xsdBundleDF = new XsdBundle(new XsdBundles(xsdTypes, Locale.getDefault()));
        final Xed xedDF = new Xed(xedNL.getDocument(), xedNL.getXsdTypes(), xsdBundleDF);
        Assert.assertEquals(Locale.getDefault(), xedDF.getLocale());
        final ResourceBundle bundleDF = xedDF.getBundle(typeInstance.getURI());
        Assert.assertNotNull(bundleDF);
        Assert.assertEquals(valueDF, bundleDF.getString(key));
        // update locale, check i18n
        final XsdBundle xsdBundleJA = new XsdBundle(new XsdBundles(xsdTypes, new Locale("ja")));
        final Xed xedJA = new Xed(xedDF.getDocument(), xedDF.getXsdTypes(), xsdBundleJA);
        Assert.assertEquals(new Locale("ja"), xedJA.getLocale());
        final ResourceBundle bundleJA = xedJA.getBundle(typeInstance.getURI());
        Assert.assertNotNull(bundleJA);
        //Assert.assertEquals(valueJA, bundleJA.getString(key));  // disable-i18n
    }
}
