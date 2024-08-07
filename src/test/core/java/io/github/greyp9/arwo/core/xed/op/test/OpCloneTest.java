package io.github.greyp9.arwo.core.xed.op.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.op.OpClone;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.pretty.DocumentPrettyU;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

public class OpCloneTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testClone() throws Exception {
        Level level = Level.FINEST;
        final URL urlInitial = ResourceU.resolve(App.Config.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        // starting document
        final byte[] xml0 = StreamU.read(ResourceU.resolve("io/github/greyp9/arwo/xml/clone/clone1.xml"));
        logger.log(level, UTF8Codec.toString(xml0));
        Assertions.assertEquals("4bb444c0", CRCU.crc32String(xml0));
        // verify Document codec
        final Document document = DocumentU.toDocument(xml0);
        final byte[] xml1a = DocumentU.toXml(document);
        final byte[] xml1b = DocumentPrettyU.toXmlPretty(document);
        logger.log(level, UTF8Codec.toString(xml1a));
        logger.log(level, UTF8Codec.toString(xml1b));
        if (SystemU.javaVersion().startsWith("1.8")) {
            Assertions.assertEquals("e8356f89", CRCU.crc32String(xml1a));
/* disable; revisit
            Assertions.assertEquals("390ab656", CRCU.crc32String(xml1b));
*/
        }
        // operate on document
        final Xed xed = new Xed(document, xsdTypes);
        final Element job = xed.getXPather().getElement("/app:app/app:cron/app:cronTab/app:cronJob");
        new OpClone().apply(job);
        // verify expected output
        final byte[] xml2a = DocumentU.toXml(document);
        final byte[] xml2b = DocumentPrettyU.toXmlPretty(document);
        logger.log(level, UTF8Codec.toString(xml2a));
        logger.log(level, UTF8Codec.toString(xml2b));
        if (SystemU.javaVersion().startsWith("1.8")) {
            Assertions.assertEquals("cc139faf", CRCU.crc32String(xml2a));
/* disable; revisit
            Assertions.assertEquals("be731769", CRCU.crc32String(xml2b));
*/
        }
        // normalize document & verify
        Document documentNormal = DocumentU.toDocument(DocumentU.toXml(document));
        final byte[] xml3a = DocumentU.toXml(documentNormal);
        final byte[] xml3b = DocumentPrettyU.toXmlPretty(documentNormal);
        logger.log(level, UTF8Codec.toString(xml3a));
        logger.log(level, UTF8Codec.toString(xml3b));
        if (SystemU.javaVersion().startsWith("1.8")) {
            Assertions.assertEquals("cc139faf", CRCU.crc32String(xml3a));
/* disable; revisit
            Assertions.assertEquals("166bbd37", CRCU.crc32String(xml3b));
*/
        }
    }
}
