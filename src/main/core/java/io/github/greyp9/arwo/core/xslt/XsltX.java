package io.github.greyp9.arwo.core.xslt;

import io.github.greyp9.arwo.core.io.ByteU;

import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

public class XsltX {
    private final byte[] xslt;

    public XsltX(final byte[] xslt) {
        this.xslt = ByteU.copy(xslt);
    }

    public final byte[] transform(final byte[] input) throws IOException {
        try {
            final TransformerFactory factory = TransformerFactory.newInstance();
            final InputStream isXSLT = new ByteArrayInputStream(xslt);
            final Templates templates = factory.newTemplates(new StreamSource(isXSLT));
            final Transformer transformer = templates.newTransformer();
            final InputStream isXML = new ByteArrayInputStream(input);
            final ByteArrayOutputStream os = new ByteArrayOutputStream();
            transformer.transform(new StreamSource(isXML), new StreamResult(os));
            return os.toByteArray();
        } catch (TransformerException e) {
            throw new IOException(e);
        }
    }
}
