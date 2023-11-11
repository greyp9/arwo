package io.github.greyp9.arwo.core.xml.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;

import java.util.logging.Logger;

public class XmlOutputTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @SuppressWarnings("checkstyle:magicnumber")
    @Test
    public void testOutputXML() throws Exception {
        // start with known ugly xml
        final byte[] xmlUgly = StreamU.read(ResourceU.resolve(Const.XML));
        logger.finest("\n" + UTF8Codec.toString(xmlUgly));
        Assertions.assertEquals(283, xmlUgly.length);
        Assertions.assertEquals("96fe8f4d", CRCU.crc32String(xmlUgly));
        // this transform reorders attributes and normalizes spacing within element declaration
        final Document document = DocumentU.toDocument(xmlUgly);
        final byte[] xmlNew = DocumentU.toXml(document);
        logger.finest("\n" + UTF8Codec.toString(xmlNew));
        if (SystemU.javaVersion().startsWith("1.8")) {
            Assertions.assertEquals(266, xmlNew.length);
            Assertions.assertEquals("2c3ef15e", CRCU.crc32String(xmlNew));
        }
    }

    public static class Const {
        public static final String XML = "io/github/greyp9/arwo/xml/ugly/ugly.xml";
    }
}
