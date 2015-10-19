package io.github.greyp9.arwo.core.xed.bundle.test;

import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundles;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.source.test.SchemaSourceAppTest;
import junit.framework.TestCase;
import org.junit.Assert;

import java.net.URL;
import java.util.Locale;
import java.util.logging.Logger;

public class BundleTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public void testLoadDefaultLocale() throws Exception {
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final XsdBundles xsdBundles = new XsdBundles(xsdTypes, Locale.getDefault());
        Bundle bundle = new Bundle(xsdBundles.getBundle("urn:arwo:realm"));
        Assert.assertNotNull(bundle);
        logger.finest(bundle.getString("realm.realmType"));
        Assert.assertEquals("Realm", bundle.getString("realm.realmType"));
        Assert.assertEquals("Principals", bundle.getString("realm.realmType.principals"));
        Assert.assertEquals("Realm Name", bundle.getString("realm.realmType.name"));
    }

    public void testLoadDE() throws Exception {
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final XsdBundles xsdBundles = new XsdBundles(xsdTypes, Locale.GERMAN);
        Bundle bundle = new Bundle(xsdBundles.getBundle("urn:arwo:realm"));
        Assert.assertNotNull(bundle);
        logger.finest(bundle.getString("realm.realmType"));
        Assert.assertEquals("Re\u00e4lm", bundle.getString("realm.realmType"));
        Assert.assertEquals("Princip\u00e4ls", bundle.getString("realm.realmType.principals"));
        Assert.assertEquals("Re\u00e4lm N\u00e4me", bundle.getString("realm.realmType.name"));
    }

    public void testLoadJP() throws Exception {
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final XsdBundles xsdBundles = new XsdBundles(xsdTypes, Locale.JAPANESE);
        Bundle bundle = new Bundle(xsdBundles.getBundle("urn:arwo:realm"));
        Assert.assertNotNull(bundle);
        logger.finest(bundle.getString("realm.realmType"));
        Assert.assertEquals("\u5c3a\u30e8\u4e39\u3057m", bundle.getString("realm.realmType"));
        Assert.assertEquals("P\u5c3a\u5de5\u51e0\u4ea1\u5de5p\u4e39\u3057\u5df1",
                bundle.getString("realm.realmType.principals"));
        Assert.assertEquals("\u5c3a\u30e8\u4e39\u3057m \u51e0\u4e39m\u30e8",
                bundle.getString("realm.realmType.name"));
    }
}
