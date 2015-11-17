package io.github.greyp9.arwo.core.xml.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;

import java.util.logging.Logger;

public class XmlOutputTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public void testOutputXML() throws Exception {
        // start with known ugly xml
        final byte[] xmlUgly = StreamU.read(ResourceU.resolve(Const.XML));
        logger.info("\n" + UTF8Codec.toString(xmlUgly));
        Assert.assertEquals(283, xmlUgly.length);
        Assert.assertEquals("96fe8f4d", CRCU.crc32String(xmlUgly));
        // this transform reorders attributes and normalizes spacing within element declaration
        final Document document = DocumentU.toDocument(xmlUgly);
        final byte[] xmlNew = DocumentU.toXml(document);
        logger.info("\n" + UTF8Codec.toString(xmlNew));
        Assert.assertEquals(266, xmlNew.length);
        Assert.assertEquals("2c3ef15e", CRCU.crc32String(xmlNew));
    }

    public static class Const {
        public static final String XML = "io/github/greyp9/arwo/xml/ugly/ugly.xml";
    }
}
