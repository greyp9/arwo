package io.github.greyp9.arwo.core.xml.priv;

import org.w3c.dom.Document;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

public final class XmlPriv {

    private XmlPriv() {
    }

    public static byte[] toXmlPretty(final Document document) throws IOException {
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        final com.sun.org.apache.xml.internal.serialize.OutputFormat outputFormat =
                new com.sun.org.apache.xml.internal.serialize.OutputFormat(document, "xml", true);
        outputFormat.setLineWidth(Const.LINE_WIDTH);
        //outputFormat.setIndenting(true);
        outputFormat.setIndent(1);
        outputFormat.setEncoding("UTF-8");
        final com.sun.org.apache.xml.internal.serialize.XMLSerializer serializer =
                new com.sun.org.apache.xml.internal.serialize.XMLSerializer(bos, outputFormat);
        serializer.serialize(document);
        return bos.toByteArray();
    }

    private static class Const {
        private static final int LINE_WIDTH = 120;
    }
}
