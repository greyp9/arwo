package io.github.greyp9.arwo.core.xed.op.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.op.OpFill;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;

import java.net.URL;
import java.util.logging.Logger;

public class OpFillTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testFillRealm() throws Exception {
        // init
        final Document document = DocumentU.createDocumentSafe(App.Realm.QNAME);
        Assertions.assertNotNull(document);
        logger.finest("\n" + DocumentU.toString(document));
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final Xed xed = new Xed(document, xsdTypes);
        // content should not be there
        Assertions.assertNull(xed.getXPather().getElement("/realm:realm/realm:principals"));
        // fill in content
        new OpFill().apply(new XedNav(xed).getRoot());
        logger.finest("\n" + DocumentU.toString(document));
        // content should be there
        Assertions.assertNotNull(xed.getXPather().getElement("/realm:realm/realm:principals"));
    }
}
