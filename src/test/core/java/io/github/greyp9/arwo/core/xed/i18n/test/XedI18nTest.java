package io.github.greyp9.arwo.core.xed.i18n.test;

import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.source.test.SchemaSourceAppTest;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Locale;
import java.util.ResourceBundle;

public class XedI18nTest extends TestCase {

    public void testRealm() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final QName nameRealm = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(typeDefinitions, false).generateEmpty(nameRealm);
        Xed xed = new Xed(document, xsdTypes, null);
        // check top-level type
        final TypeInstance typeInstance = typeDefinitions.getElementTypes().get(nameRealm.toString());
        Assert.assertNotNull(typeInstance);
        // i18n test values
        String key = "realm.realmType";
        String valueDF = "Realm";
        String valueJA = "\u5c3a\u30e8\u4e39\u3057m";
        // check i18n (should be default, as no locale specified)
        ResourceBundle bundleNull = xed.getBundle(typeInstance.getURI());
        Assert.assertNull(bundleNull);
        // update locale, check i18n
        xed = new XedFactory().update(xed, Locale.getDefault());
        Assert.assertEquals(Locale.getDefault(), xed.getLocale());
        ResourceBundle bundleDF = xed.getBundle(typeInstance.getURI());
        Assert.assertNotNull(bundleDF);
        Assert.assertEquals(valueDF, bundleDF.getString(key));
        // update locale, check i18n
        xed = new XedFactory().update(xed, new Locale("ja"));
        Assert.assertEquals(new Locale("ja"), xed.getLocale());
        ResourceBundle bundleJA = xed.getBundle(typeInstance.getURI());
        Assert.assertNotNull(bundleJA);
        Assert.assertEquals(valueJA, bundleJA.getString(key));
    }
}
