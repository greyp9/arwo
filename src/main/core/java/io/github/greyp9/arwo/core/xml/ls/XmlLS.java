package io.github.greyp9.arwo.core.xml.ls;

import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.Document;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSSerializer;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

public final class XmlLS {

    private XmlLS() {
    }

    public static byte[] toXmlPretty(final Document document) throws IOException {
        try {
            final ByteArrayOutputStream bos = new ByteArrayOutputStream();
            final DOMImplementationRegistry registry = DOMImplementationRegistry.newInstance();
            final DOMImplementationLS loadSave = (DOMImplementationLS) registry.getDOMImplementation("LS");  // i18n
            final LSOutput output = loadSave.createLSOutput();
            output.setByteStream(bos);
            final LSSerializer serializer = loadSave.createLSSerializer();
            final DOMConfiguration config = serializer.getDomConfig();
            config.setParameter("format-pretty-print", true);  // i18n
            serializer.write(document, output);
            return bos.toByteArray();
        } catch (ClassNotFoundException e) {
            throw new IOException(e);
        } catch (InstantiationException e) {
            throw new IOException(e);
        } catch (IllegalAccessException e) {
            throw new IOException(e);
        }
    }
}
