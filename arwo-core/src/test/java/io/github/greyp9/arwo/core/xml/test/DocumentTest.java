package io.github.greyp9.arwo.core.xml.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.logging.Logger;

public class DocumentTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    void testCreateElement() throws IOException {
        final Document document = DocumentU.createDocument("cookies", "urn:arwo:cookie");
        final Element elementCookies = document.getDocumentElement();
        final Element elementCookie = ElementU.addElement(elementCookies, "cookie", null, NTV.create());
        Assertions.assertEquals(elementCookies, elementCookie.getParentNode());
        logger.finest(UTF8Codec.toString(DocumentU.toXml(document.getDocumentElement())));
    }

    @Test
    void testXhtmlTemplate() throws IOException {
        // parent document
        final Document documentHtml = DocumentU.toDocument(StreamU.read(ResourceU.resolve(App.Html.UI)));
        final Element body = new XPather(documentHtml, null).getElement(Html.XPath.BODY);
        Assertions.assertNotNull(body);
        Assertions.assertEquals(0, body.getChildNodes().getLength());
        // child document
        final Document documentDiv = DocumentU.createDocument("div", null);
        final Element documentElement = documentDiv.getDocumentElement();
        ElementU.importNode(documentElement, body);
        Assertions.assertEquals(1, body.getChildNodes().getLength());
        Assertions.assertEquals("div", body.getChildNodes().item(0).getNodeName());
        logger.finest(UTF8Codec.toString(DocumentU.toXml(documentHtml.getDocumentElement())));
    }
}
