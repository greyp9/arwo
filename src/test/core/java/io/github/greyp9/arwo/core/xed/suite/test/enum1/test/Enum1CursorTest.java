package io.github.greyp9.arwo.core.xed.suite.test.enum1.test;

import io.github.greyp9.arwo.core.app.test.TestApp;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundles;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.XedCursorView;
import io.github.greyp9.arwo.core.xed.view.text.CursorTextView;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
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

public class Enum1CursorTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testEnum() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_ENUM1);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:enum1}folder");
        // generate document
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        logger.finest("\n" + DocumentU.toString(document));
        final Xed xed = new Xed(document, xsdTypes);
        final XsdBundle xsdBundle = new XsdBundle(new XsdBundles(xsdTypes, Locale.getDefault()));
        final Xed xedUI = new Xed(xed.getDocument(), xed.getXsdTypes(), xsdBundle);
        // validate
        final Collection<String> messages0 = xed.validate();
        Assertions.assertEquals("[]", messages0.toString());
        // navigate
        final Element folder = xed.getXPather().getElement("/e1:folder");
        Assertions.assertNotNull(folder);
        final XedCursor cursorFolder = new XedNav(xedUI).find(folder);
        Assertions.assertNotNull(cursorFolder);
        Assertions.assertEquals("/", cursorFolder.getURI());
        // insert
        final XedCursor cursorFileType = new XedNav(xedUI).find("file", cursorFolder);
        Assertions.assertNotNull(cursorFileType);
        Assertions.assertEquals("/058a2/", cursorFileType.getURI());
        final NameTypeValues ntv1 = HttpArguments.toArguments("name=foo.txt&type=text&hidden=false");
        final ValueInstance value1 = ValueInstance.create(cursorFileType.getTypeInstance(), ntv1);
        final Element file1 = xedUI.create(cursorFileType.getParent().getElement(), value1);
        Assertions.assertNotNull(file1);
        // navigate
        final XedCursor cursorFile1 = new XedNav(xedUI).find(file1);
        Assertions.assertNotNull(cursorFile1);
        Assertions.assertEquals("/058a2/0f33b/", cursorFile1.getURI());
        // validate
        logger.finest("\n" + DocumentU.toString(document));
        final Collection<String> messages1 = xedUI.validate();
        Assertions.assertEquals("[]", messages1.toString());
        // view
        final String renderFileType = new CursorTextView(new XedCursorView(cursorFileType)).render();
        logger.finest("FileType\n" + renderFileType);
        Assertions.assertEquals("3c2f337d", CRCU.crc32String(UTF8Codec.toBytes(renderFileType)));
        // view
        final String renderFile = new CursorTextView(new XedCursorView(cursorFile1)).render();
        logger.finest("File\n" + renderFile);
        Assertions.assertEquals("19ceb9d5", CRCU.crc32String(UTF8Codec.toBytes(renderFile)));
    }
}
