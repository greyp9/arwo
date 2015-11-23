package io.github.greyp9.arwo.core.xed.xsd.noNs.test;

import io.github.greyp9.arwo.core.app.test.TestApp;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
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

public class NoNsOpTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testNavigate() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_NO_NS);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("document");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        logger.finest(DocumentU.toString(xed.getDocument()));
        // navigate
        final Element documentE = xed.getXPather().getElement("/document");
        final XedCursor cursorDocumentByNode = new XedNav(xed).find(documentE);
        Assert.assertEquals("/", cursorDocumentByNode.getURI());
        final XedCursor cursorDocumentByPath = new XedNav(xed).find("/");
        Assert.assertEquals(cursorDocumentByNode.getElement(), cursorDocumentByPath.getElement());
        // navigate
        XedCursor cursorFilenameType = new XedNav(xed).find("file", cursorDocumentByNode);
        Assert.assertEquals("/668f4/", cursorFilenameType.getURI());
    }

    public void testCreate() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_NO_NS);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("document");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        logger.finest(DocumentU.toString(xed.getDocument()));
        // navigate
        final Element documentE = xed.getXPather().getElement("/document");
        final XedCursor cursorDocument = new XedNav(xed).find(documentE);
        // insert
        final XedCursor cursorFileType = new XedNav(xed).find("file", cursorDocument);
        final NameTypeValues ntv1 = HttpArguments.toArguments("");
        final ValueInstance value1 = ValueInstance.create(cursorFileType.getTypeInstance(), ntv1);
        final Element file1 = xed.create(cursorFileType.getParent().getElement(), value1);
        Assert.assertNotNull(file1);
        final Collection<String> messages1 = xed.validate();
        Assert.assertEquals("[]", messages1.toString());
        // insert
        final XedCursor cursorFile = new XedNav(xed).find(file1);
        Assert.assertEquals("/668f4/ace47/", cursorFile.getURI());
        final XedCursor cursorIncludeType = new XedNav(xed).find("include", cursorFile);
        Assert.assertEquals("/668f4/ace47/2bce1/", cursorIncludeType.getURI());
        final NameTypeValues ntv2 = HttpArguments.toArguments("include=include1");
        final ValueInstance value2 = ValueInstance.create(cursorIncludeType.getTypeInstance(), ntv2);
        final Element include2 = xed.create(cursorIncludeType.getParent().getElement(), value2);
        Assert.assertNotNull(include2);
        final Collection<String> messages2 = xed.validate();
        Assert.assertEquals("[]", messages2.toString());
        // insert
        final XedCursor cursorExcludeType = new XedNav(xed).find("exclude", cursorFile);
        Assert.assertEquals("/668f4/ace47/13592/", cursorExcludeType.getURI());
        final NameTypeValues ntv3 = HttpArguments.toArguments("exclude=exclude1");
        final ValueInstance value3 = ValueInstance.create(cursorExcludeType.getTypeInstance(), ntv3);
        final Element exclude3 = xed.create(cursorExcludeType.getParent().getElement(), value3);
        Assert.assertNotNull(exclude3);
        final Collection<String> messages3 = xed.validate();
        Assert.assertEquals("[]", messages3.toString());
        // validate
        logger.finest(DocumentU.toString(document));
        final byte[] xml = DocumentU.toXml(document);
        Assert.assertEquals(149, xml.length);
        Assert.assertEquals("3176067a", CRCU.crc32String(xml));
    }

    public void testCreateUpdate() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_NO_NS);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("document");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        logger.finest(DocumentU.toString(xed.getDocument()));
        // navigate
        final Element documentE = xed.getXPather().getElement("/document");
        final XedCursor cursorDocument = new XedNav(xed).find(documentE);
        // insert
        final XedCursor cursorFileType = new XedNav(xed).find("file", cursorDocument);
        final NameTypeValues ntv1 = HttpArguments.toArguments("");
        final ValueInstance value1 = ValueInstance.create(cursorFileType.getTypeInstance(), ntv1);
        final Element file1 = xed.create(cursorFileType.getParent().getElement(), value1);
        Assert.assertNotNull(file1);
        final Collection<String> messages1 = xed.validate();
        Assert.assertEquals("[]", messages1.toString());
        // insert
        final XedCursor cursorFile = new XedNav(xed).find(file1);
        Assert.assertEquals("/668f4/ace47/", cursorFile.getURI());
        final XedCursor cursorIncludeType = new XedNav(xed).find("include", cursorFile);
        Assert.assertEquals("/668f4/ace47/2bce1/", cursorIncludeType.getURI());
        final NameTypeValues ntv2 = HttpArguments.toArguments("include=include1");
        final ValueInstance value2 = ValueInstance.create(cursorIncludeType.getTypeInstance(), ntv2);
        final Element include2 = xed.create(cursorIncludeType.getParent().getElement(), value2);
        Assert.assertNotNull(include2);
        final Collection<String> messages2 = xed.validate();
        Assert.assertEquals("[]", messages2.toString());
        // update
        final XedCursor cursorInclude = new XedNav(xed).find(include2);
        final NameTypeValues ntv3 = HttpArguments.toArguments("include=include3");
        final ValueInstance value3 = ValueInstance.create(cursorIncludeType.getTypeInstance(), ntv3);
        final Element include3 = xed.update(cursorInclude.getElement(), value3);
        Assert.assertNotNull(include3);
        final Collection<String> messages3 = xed.validate();
        Assert.assertEquals("[]", messages3.toString());
        // validate
        logger.finest(DocumentU.toString(document));
        final byte[] xml = DocumentU.toXml(document);
        Assert.assertEquals(121, xml.length);
        Assert.assertEquals("ea9eccbe", CRCU.crc32String(xml));
    }
}
