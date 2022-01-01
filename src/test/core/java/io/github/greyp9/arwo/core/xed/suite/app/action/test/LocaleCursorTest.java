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
import io.github.greyp9.arwo.core.xsd.data.DataTypeRestrictions;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;

import java.net.URL;
import java.util.Locale;
import java.util.logging.Logger;

public class LocaleCursorTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testLocaleText() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Actions.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeInstance localeType = xsdTypes.getElementType(App.Actions.QNAME_LOCALE.toString());
        final DataTypeRestrictions restrictions = localeType.getDataType().getRestrictions();
        Assert.assertEquals(6, restrictions.getEnumValues().size());
        // generate document
        final DocumentFactory documentFactory = new DocumentFactory(xsdTypes.getTypeDefinitions(), false);
        final Document document = documentFactory.generateEmpty(App.Actions.QNAME_LOCALE);
        logger.finest(DocumentU.toString(document));
        // validate
        final XsdBundle xsdBundle = new XsdBundle(new XsdBundles(xsdTypes, Locale.getDefault()));
        final Xed xed = new Xed(document, xsdTypes, xsdBundle);
        final XedCursor cursor = new XedNav(xed).getRoot();
        Assert.assertEquals(localeType, cursor.getTypeInstance());
        final String pageText = new PropertyPageTextView(new XedPropertyPageView(null, cursor)).render();
        logger.finest(pageText);
        Assert.assertEquals("599e073f", CRCU.crc32String(UTF8Codec.toBytes(pageText)));
    }
}
