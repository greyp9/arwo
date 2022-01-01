package io.github.greyp9.arwo.core.xed.op.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.op.OpPrune;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.net.URL;
import java.util.logging.Logger;

public class OpPruneTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testPruneRealm() throws Exception {
        // init
        final Document document = DocumentU.createDocumentSafe(App.Realm.QNAME);
        Assert.assertNotNull(document);
        // add spurious content
        final Element element = document.getDocumentElement();
        ElementU.setAttribute(element, "foo-attr", "bar");
        ElementU.addElement(element, "foo-element", "bar");
        logger.finest("\n" + DocumentU.toString(document));
        // generate model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final Xed xed = new Xed(document, xsdTypes);
        final XPather xpather = xed.getXPather();
        // content should be there
        Assert.assertEquals(1, xpather.getAttributes("/realm:realm/@foo-attr").size());
        Assert.assertEquals(1, xpather.getElements("/realm:realm/realm:foo-element").size());
        // prune content
        new OpPrune().apply(new XedNav(xed).getRoot());
        logger.finest("\n" + DocumentU.toString(document));
        // content should not be there
        Assert.assertEquals(0, xpather.getAttributes("/realm:realm/@foo-attr").size());
        Assert.assertEquals(0, xpather.getElements("/realm:realm/realm:foo-element").size());
    }
}
