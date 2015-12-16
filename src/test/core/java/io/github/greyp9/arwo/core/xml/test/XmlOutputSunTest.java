package io.github.greyp9.arwo.core.xml.test;

import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.logging.Logger;

public class XmlOutputSunTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public void testOutputXMLProject() throws Exception {
        // start with known ugly xml
        final byte[] xmlUgly = StreamU.read(ResourceU.resolve(Const.XML));
        logger.finest("\n" + UTF8Codec.toString(xmlUgly));
        Assert.assertEquals(283, xmlUgly.length);
        Assert.assertEquals("96fe8f4d", CRCU.crc32String(xmlUgly));
        // this transform pretty prints
        final Document document = DocumentU.toDocument(xmlUgly);
        final byte[] xmlNew = DocumentU.toXmlPretty(document);
        logger.finest("\n" + UTF8Codec.toString(xmlNew));
        Assert.assertEquals(248, xmlNew.length);
        Assert.assertEquals("c26f702f", CRCU.crc32String(xmlNew));
    }

    public void testOutputXML() throws Exception {
        // start with known ugly xml
        final byte[] xmlUgly = StreamU.read(ResourceU.resolve(Const.XML));
        logger.finest("\n" + UTF8Codec.toString(xmlUgly));
        Assert.assertEquals(283, xmlUgly.length);
        Assert.assertEquals("96fe8f4d", CRCU.crc32String(xmlUgly));
        // this transform pretty prints
        final Document document = DocumentU.toDocument(xmlUgly);
        final byte[] xmlNew = toXmlPrettyOF(document);
        logger.finest("\n" + UTF8Codec.toString(xmlNew));
        Assert.assertEquals(248, xmlNew.length);
        Assert.assertEquals("c26f702f", CRCU.crc32String(xmlNew));
    }

    private static byte[] toXmlPrettyOF(Document document) throws IOException {
        //document.normalizeDocument();
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        OutputFormat outputFormat = new OutputFormat(document, "xml", true);
        outputFormat.setLineWidth(120);
        //outputFormat.setIndenting(true);
        outputFormat.setIndent(1);
        outputFormat.setEncoding(UTF8Codec.Const.UTF8);
        XMLSerializer serializer = new XMLSerializer(bos, outputFormat);
        serializer.serialize(document);
        return bos.toByteArray();
    }

    public static class Const {
        public static final String XML = "io/github/greyp9/arwo/xml/ugly/ugly.xml";
    }
}
