package io.github.greyp9.arwo.core.xed.suite.app.action.test;

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
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;

import java.net.URL;
import java.util.Locale;
import java.util.logging.Logger;

public class LocaleI18nTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testName() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Actions.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final DocumentFactory documentFactory = new DocumentFactory(xsdTypes.getTypeDefinitions(), false);
        final Document document = documentFactory.generateEmpty(App.Actions.QNAME_LOCALE);
        logger.finest(DocumentU.toString(document));
        // validate
        final XsdBundle xsdBundleDefault = new XsdBundle(new XsdBundles(xsdTypes, Locale.getDefault()));
        final XsdBundle xsdBundle = new XsdBundle(new XsdBundles(xsdTypes, new Locale("ru")));
        final Xed xed = new Xed(document, xsdTypes, xsdBundle);
        final XedCursor cursor = new XedNav(xed).getRoot();
        final String pageText = new PropertyPageTextView(new XedPropertyPageView(null, cursor)).render();
        logger.finest(pageText);
        // pseudo i18n bundles are assembled by Maven antrun plugin, which runs on successful build
        // if i18n bundles are available, expect RU text; otherwise, expect default locale text
        final TypeInstance typeInstance = cursor.getTypeInstance();
        final boolean isAvailableI18n =
                !xsdBundle.getLabel(typeInstance).equals(xsdBundleDefault.getLabel(typeInstance));
        final String expected = isAvailableI18n ? "0e07d15d" : "599e073f";
        Assertions.assertEquals(expected, CRCU.crc32String(UTF8Codec.toBytes(pageText)));
    }
}
