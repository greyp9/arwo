package io.github.greyp9.arwo.core.xslt;

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
    private final Transformer transformer;

    public XsltX(final byte[] xslt) throws IOException {
        try {
            final TransformerFactory factory = TransformerFactory.newInstance();
            final InputStream isXSLT = new ByteArrayInputStream(xslt);
            final Templates templates = factory.newTemplates(new StreamSource(isXSLT));
            this.transformer = templates.newTransformer();
        } catch (TransformerException e) {
            throw new IOException(e);
        }
    }

    public final byte[] transform(final byte[] input) throws IOException {
        try {
            final InputStream isXML = new ByteArrayInputStream(input);
            final ByteArrayOutputStream os = new ByteArrayOutputStream();
            transformer.transform(new StreamSource(isXML), new StreamResult(os));
            return os.toByteArray();
        } catch (TransformerException e) {
            throw new IOException(e);
        }
    }
}
