package io.github.greyp9.arwo.core.xml;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

public final class DocumentU {

    private DocumentU() {
    }

    public static String toString(final Node node) throws IOException {
        return io.github.greyp9.arwo.core.charset.UTF8Codec.toString(toXml(node));
    }

    public static byte[] toXml(final Node node) throws IOException {
        return (node == null) ? null : toXmlNN(node);
    }

    private static byte[] toXmlNN(final Node node) throws IOException {
        final Source source = new DOMSource(node);
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        final Result result = new StreamResult(bos);
        final TransformerFactory factory = TransformerFactory.newInstance();
        try {
            final Transformer transformer = factory.newTransformer();
            //transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
            //transformer.setOutputProperty(OutputKeys.METHOD, "xml");
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            //transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
            //transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
            transformer.transform(source, result);
            return bos.toByteArray();
        } catch (TransformerException e) {
            throw new IOException(e);
        }
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

    @SuppressWarnings("PMD.OnlyOneReturn")
    public static Document createDocumentSafe(final String name, final String namespace) {
        try {
            return createDocument(name, namespace);
        } catch (IOException e) {
            return null;
        }
    }

    public static Document createDocument(final String name, final String namespace) throws IOException {
        final DocumentBuilderFactory builderFactory = DocumentBuilderFactory.newInstance();
        try {
            final DocumentBuilder builder = builderFactory.newDocumentBuilder();
            builderFactory.setFeature(Const.LOAD_EXTERNAL_DTD, false);
            builderFactory.setNamespaceAware(true);
            final Document document = builder.newDocument();
            document.appendChild(createElement(document, name, namespace));
            return document;
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        }
    }

    private static Element createElement(final Document document, final String name, final String namespace) {
        return document.createElementNS(namespace, name);
    }

    private static class Const {
        private static final String LOAD_EXTERNAL_DTD =
                "http://apache.org/xml/features/nonvalidating/load-external-dtd";
    }
}
