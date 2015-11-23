package io.github.greyp9.arwo.core.xed.op.test;

import io.github.greyp9.arwo.core.app.test.TestApp;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.op.OpOrder;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.logging.Logger;

public class OpOrderChoiceTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public void testOrderConcrete() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_CHOICE2);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final QName qname = QNameU.getQName("{urn:arwo:choice2}job");
        final Document document = new DocumentFactory(typeDefinitions, true).generateEmpty(qname);
        logger.finest(DocumentU.toString(document));
        final Xed xed = new Xed(document, xsdTypes, null);
        final Collection<String> validate0 = xed.validate();
        Assert.assertEquals("[]", validate0.toString());
        // scramble
        final Element container = xed.getXPather().getElement("/ch2:job");
        Assert.assertNotNull(container);
        final List<Element> children = new ArrayList<Element>(ElementU.getChildren(container));
        for (Element child : children) {
            ElementU.detach(child);
        }
        Collections.shuffle(children, new Random(System.currentTimeMillis()));
        for (Element child : children) {
            ElementU.addElement(container, child, null);
        }
        // check
        logger.finest(DocumentU.toString(document));
        final Collection<String> validate1 = xed.validate();
        Assert.assertTrue(validate1.size() > 0);
        // apply schema order
        final XedNav nav = new XedNav(xed);
        final XedCursor cursorContainer = nav.find(container);
        new OpOrder().apply(cursorContainer);
        // check
        logger.finest(DocumentU.toString(document));
        final Collection<String> validate2 = xed.validate();
        Assert.assertEquals("[]", validate2.toString());
    }
}
