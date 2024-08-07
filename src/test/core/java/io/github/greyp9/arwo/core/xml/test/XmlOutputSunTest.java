package io.github.greyp9.arwo.core.xml.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.pretty.DocumentPrettyU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;

import java.util.logging.Logger;

@SuppressWarnings("checkstyle:magicnumber")
public class XmlOutputSunTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testOutputXMLProject() throws Exception {
        // start with known ugly xml
        final byte[] xmlUgly = StreamU.read(ResourceU.resolve(Const.XML));
        logger.finest("\n" + UTF8Codec.toString(xmlUgly));
        Assertions.assertEquals(283, xmlUgly.length);
        Assertions.assertEquals("96fe8f4d", CRCU.crc32String(xmlUgly));
        // this transform pretty prints
        final Document document = DocumentU.toDocument(xmlUgly);
        final byte[] xmlNew = DocumentPrettyU.toXmlPretty(document);
        logger.finest("\n" + UTF8Codec.toString(xmlNew));
/* disable; revisit
        if (SystemU.javaVersion().startsWith("1.8")) {
            Assertions.assertEquals(248, xmlNew.length);
            Assertions.assertEquals("c26f702f", CRCU.crc32String(xmlNew));
        }
*/
    }

    @Test
    public void testOutputXML() throws Exception {
        // start with known ugly xml
        final byte[] xmlUgly = StreamU.read(ResourceU.resolve(Const.XML));
        logger.finest("\n" + UTF8Codec.toString(xmlUgly));
        Assertions.assertEquals(283, xmlUgly.length);
        Assertions.assertEquals("96fe8f4d", CRCU.crc32String(xmlUgly));
        // this transform pretty prints
        final Document document = DocumentU.toDocument(xmlUgly);
/*
        final byte[] xmlNew = toXmlPrettyOF(document);
        logger.finest("\n" + UTF8Codec.toString(xmlNew));
        Assertions.assertEquals(248, xmlNew.length);
        Assertions.assertEquals("c26f702f", CRCU.crc32String(xmlNew));
*/
        final byte[] xmlNew = DocumentPrettyU.toXmlPretty(document);
        logger.finest("\n" + UTF8Codec.toString(xmlNew));
/*
        Assertions.assertEquals(253, xmlNew.length);
        Assertions.assertEquals("40cabef1", CRCU.crc32String(xmlNew));
*/
    }

/* unsupported in JRE 9+
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
*/

    public static class Const {
        public static final String XML = "io/github/greyp9/arwo/xml/ugly/ugly.xml";
    }
}
