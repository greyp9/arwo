package io.github.greyp9.arwo.core.xsd.validate;

import io.github.greyp9.arwo.core.io.ByteU;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;

public class Validator {
    private final InputSource inputSource;

    public Validator(final byte[] xsd) {
        inputSource = new InputSource(new ByteArrayInputStream(ByteU.copy(xsd)));
    }

    public Validator(final URL url) throws MalformedURLException {
        inputSource = new InputSource(url.toExternalForm());
    }

    public Validator(final URL url, final byte[] xsd) throws MalformedURLException {
        inputSource = new InputSource(url.toExternalForm());
        inputSource.setByteStream(new ByteArrayInputStream(ByteU.copy(xsd)));
    }

    public Validator(final File fileXsd) throws MalformedURLException {
        inputSource = new InputSource(fileXsd.toURI().toURL().toExternalForm());
    }

    public final Collection<String> validate(final byte[] xml) throws IOException {
        final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        factory.setValidating(true);
        factory.setAttribute(Const.SCHEMA_LANGUAGE, Const.XMLSCHEMA);
        factory.setAttribute(Const.SCHEMA_SOURCE, inputSource);
        final ErrorHandler errorHandler = new ErrorHandler();
        try {
            final DocumentBuilder builder = factory.newDocumentBuilder();
            builder.setErrorHandler(errorHandler);
            builder.parse(new InputSource(new ByteArrayInputStream(xml)));
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        } catch (SAXException e) {
            throw new IOException(e);
        }
        return errorHandler.getMessages();
    }

    private static class Const {
        private static final String SCHEMA_LANGUAGE = "http://java.sun.com/xml/jaxp/properties/schemaLanguage";
        private static final String SCHEMA_SOURCE = "http://java.sun.com/xml/jaxp/properties/schemaSource";
        private static final String XMLSCHEMA = "http://www.w3.org/2001/XMLSchema";
    }
}
