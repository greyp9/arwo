package io.github.greyp9.arwo.core.xed.suite.app.realm.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundles;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.text.PropertyPageTextView;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;

import java.net.URL;
import java.util.Locale;
import java.util.logging.Logger;

public class RealmI18nTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testName() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final DocumentFactory documentFactory = new DocumentFactory(xsdTypes.getTypeDefinitions(), false);
        final Document document = documentFactory.generateEmpty(App.Realm.QNAME);
        logger.finest(DocumentU.toString(document));
        // validate
        final XsdBundle xsdBundle = new XsdBundle(new XsdBundles(xsdTypes, new Locale("ru")));
        final Xed xed = new Xed(document, xsdTypes, xsdBundle);
        final XedCursor cursor = new XedNav(xed).getRoot();
        final String pageText = new PropertyPageTextView(new XedPropertyPageView(null, cursor)).render();
        logger.finest(pageText);
        Assert.assertEquals("c3fb83e3", CRCU.crc32String(UTF8Codec.toBytes(pageText)));
    }
}
