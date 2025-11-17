package io.github.greyp9.arwo.core.action.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.net.URL;
import java.util.logging.Logger;

public class ActionFilterTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    void testFilter() throws IOException {
        final URL urlInitial = ResourceU.resolve(App.Actions.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:action}filter");
        // generate document
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        logger.finest(UTF8Codec.toString(DocumentU.toXml(document, DocumentU.transformerConfigXml(true))));
    }
}
