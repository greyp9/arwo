package io.github.greyp9.arwo.core.xed.clip.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.clip.XedClipboard;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.op.OpClipClear;
import io.github.greyp9.arwo.core.xed.op.OpClipCopy;
import io.github.greyp9.arwo.core.xed.op.OpClipCut;
import io.github.greyp9.arwo.core.xed.op.OpClipPaste;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.logging.Logger;

public class ClipboardTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testClipboardRealm() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        // generate document
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), true).generateEmpty(qname);
        logger.finest(DocumentU.toString(document));
        // initialize context
        final Xed xed = new Xed(document, xsdTypes);
        final XedNav nav = new XedNav(xed);
        final XPather xpather = xed.getXPather();
        final Element principals = xpather.getElement("/realm:realm/realm:principals");
        final Element principal = xpather.getElement("/realm:realm/realm:principals/realm:principal");
        Assert.assertNotNull(principal);
        // initialize context
        final XedClipboard clipboard = new XedClipboard();
        Assert.assertEquals(0, clipboard.getFragments().size());
        final XedCursor cursorPrincipal = nav.find(principal);
        Assert.assertNotNull(cursorPrincipal);
        Assert.assertEquals("/ecd28/7256d/8dc37/c3dce/", cursorPrincipal.getURI());
        final XedCursor cursorPrincipalType = cursorPrincipal.getParent();
        Assert.assertEquals("/ecd28/7256d/8dc37/", cursorPrincipalType.getURI());
        // copy/cut
        Assert.assertEquals(1, ElementU.getChildren(principals).size());
        new OpClipCopy(clipboard).copy(cursorPrincipal);
        new OpClipCut(clipboard).cut(cursorPrincipal);
        logger.finest(DocumentU.toString(document));
        Assert.assertEquals(0, ElementU.getChildren(principals).size());
        // paste
        new OpClipPaste(clipboard).paste(cursorPrincipalType);
        logger.finest(DocumentU.toString(document));
        Assert.assertEquals(2, ElementU.getChildren(principals).size());
        new OpClipPaste(clipboard).paste(cursorPrincipalType);
        logger.finest(DocumentU.toString(document));
        final int sizeExpected = 4;
        Assert.assertEquals(sizeExpected, ElementU.getChildren(principals).size());
        // paste empty
        new OpClipClear(clipboard).clear();
        new OpClipPaste(clipboard).paste(cursorPrincipalType);
        logger.finest(DocumentU.toString(document));
        Assert.assertEquals(sizeExpected, ElementU.getChildren(principals).size());
    }
}
