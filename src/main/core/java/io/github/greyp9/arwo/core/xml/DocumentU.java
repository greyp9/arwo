package io.github.greyp9.arwo.core.xml;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.ByteArrayInputStream;
import java.io.IOException;

public final class DocumentU {

    private DocumentU() {
    }

    public static Document toDocument(final byte[] xml) throws IOException {
        final DocumentBuilderFactory builderFactory = DocumentBuilderFactory.newInstance();
        try {
            builderFactory.setFeature(Const.LOAD_EXTERNAL_DTD, false);
            builderFactory.setNamespaceAware(true);
            final DocumentBuilder builder = builderFactory.newDocumentBuilder();
            return builder.parse(new ByteArrayInputStream(xml));
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        } catch (SAXException e) {
            throw new IOException(e);
        }
    }

    private static class Const {
        private static final String LOAD_EXTERNAL_DTD =
                "http://apache.org/xml/features/nonvalidating/load-external-dtd";
    }
}
