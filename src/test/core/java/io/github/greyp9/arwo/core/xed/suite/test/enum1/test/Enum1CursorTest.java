package io.github.greyp9.arwo.core.xed.suite.test.enum1.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
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
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Collection;
import java.util.logging.Logger;

public class Enum1CursorTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testEnum() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(Const.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:enum1}folder");
        // generate document
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        logger.finest("\n" + DocumentU.toString(document));
        final Xed xed = new Xed(document, xsdTypes);
        // validate
        final Collection<String> messages0 = xed.validate();
        Assert.assertEquals("[]", messages0.toString());
        // navigate
        final Element folder = xed.getXPather().getElement("/e1:folder");
        Assert.assertNotNull(folder);
        final XedCursor cursorFolder = new XedNav(xed).find(folder);
        Assert.assertNotNull(cursorFolder);
        Assert.assertEquals("/", cursorFolder.getURI());
        // insert
        final XedCursor cursorFileType = new XedNav(xed).find("file", cursorFolder);
        Assert.assertNotNull(cursorFileType);
        Assert.assertEquals("/058a2/", cursorFileType.getURI());
        final NameTypeValues ntv1 = HttpArguments.toArguments("name=foo.txt&type=text");
        final ValueInstance value1 = ValueInstance.create(cursorFileType.getTypeInstance(), ntv1);
        final Element file1 = xed.create(cursorFileType.getParent().getElement(), value1);
        Assert.assertNotNull(file1);
        // navigate
        final XedCursor cursorFile1 = new XedNav(xed).find(file1);
        Assert.assertNotNull(cursorFile1);
        Assert.assertEquals("/058a2/0f33b/", cursorFile1.getURI());
        // validate
        logger.finest("\n" + DocumentU.toString(document));
        final Collection<String> messages1 = xed.validate();
        Assert.assertEquals("[]", messages1.toString());
        // view
        final String renderFileType = new CursorTextView(new XedCursorView(cursorFileType)).render();
        logger.finest("FileType\n" + renderFileType);
        Assert.assertEquals("e87364e1", CRCU.crc32String(UTF8Codec.toBytes(renderFileType)));
        // view
        final String renderFile = new CursorTextView(new XedCursorView(cursorFile1)).render();
        logger.finest("File\n" + renderFile);
        Assert.assertEquals("72d84bbd", CRCU.crc32String(UTF8Codec.toBytes(renderFile)));
    }

    public static class Const {
        public static final String XSD = "io/github/greyp9/arwo/xsd/enum1/enum1.xsd";
    }
}
