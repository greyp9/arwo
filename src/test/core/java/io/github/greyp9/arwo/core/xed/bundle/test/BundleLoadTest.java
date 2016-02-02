package io.github.greyp9.arwo.core.xed.bundle.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.time.Stopwatch;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundles;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import junit.framework.TestCase;
import org.junit.Assert;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.net.URL;
import java.util.Locale;
import java.util.logging.Logger;

public class BundleLoadTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public void testLoadFactory() throws Exception {
        for (int j = 0; (j < 3); ++j) {
            final XedFactory factory0 = new XedFactory();
            final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
            final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
            final Stopwatch stopwatch = new Stopwatch();
            final Xed xed = factory0.generateEmpty(urlInitial, qname, null);
            Assert.assertNotNull(xed);
            final long lap1 = stopwatch.lap();
            for (int i = 0; (i < 2); ++i) {
                final Xed xed1 = factory0.generateEmpty(urlInitial, qname, null);
                Assert.assertNotNull(xed1);
            }
            final long lap2 = stopwatch.lap();
            logger.info(Long.toString(lap1));
            logger.info(Long.toString(lap2));
        }
    }

    public void testLoadDefaultLocale() throws Exception {
        for (int i = 0; (i < 2); ++i) {
            Bundle bundle = getBundle();
            Assert.assertNotNull(bundle);
            logger.info(bundle.getString("realm.realmType"));
        }
    }

    private Bundle getBundle() throws IOException {
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final XsdBundles xsdBundles = new XsdBundles(xsdTypes, Locale.getDefault());
        return new Bundle(xsdBundles.getBundle("urn:arwo:realm"));
    }
}
