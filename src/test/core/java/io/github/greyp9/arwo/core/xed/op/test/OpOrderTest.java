package io.github.greyp9.arwo.core.xed.op.test;

import io.github.greyp9.arwo.core.app.test.TestApp;
import io.github.greyp9.arwo.core.lang.SystemU;
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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
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

public class OpOrderTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testOrderConcrete() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_ORDER);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final QName qname = QNameU.getQName("{urn:arwo:order}container");
        final Document document = new DocumentFactory(typeDefinitions, false).generateEmpty(qname);
        logger.finest(DocumentU.toString(document));
        final Xed xed = new Xed(document, xsdTypes);
        final Collection<String> validate0 = xed.validate();
        Assertions.assertEquals("[]", validate0.toString());
        // scramble
        final Element container = xed.getXPather().getElement("/o:container");
        Assertions.assertNotNull(container);
        final List<Element> children = new ArrayList<Element>(ElementU.getChildren(container));
        for (Element child : children) {
            ElementU.detach(child);
        }
        Collections.shuffle(children, new Random(SystemU.currentTimeMillis()));
        for (Element child : children) {
            ElementU.addElement(container, child, null);
        }
        // check
        logger.finest(DocumentU.toString(document));
        final Collection<String> validate1 = xed.validate();
        Assertions.assertTrue(validate1.size() > 0);
        // apply schema order
        final XedNav nav = new XedNav(xed);
        final XedCursor cursorContainer = nav.find(container);
        new OpOrder().apply(cursorContainer);
        // check
        logger.finest(DocumentU.toString(document));
        final Collection<String> validate2 = xed.validate();
        Assertions.assertEquals("[]", validate2.toString());
    }
}
