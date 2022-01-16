package io.github.greyp9.arwo.core.xml.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.DOMStringList;
import org.w3c.dom.Document;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSOutput;
import org.w3c.dom.ls.LSSerializer;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.logging.Logger;

public class XmlOutputLSTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @SuppressWarnings("checkstyle:magicnumber")
    @Test
    public void testOutputXML() throws Exception {
        // start with known ugly xml
        final byte[] xmlUgly = StreamU.read(ResourceU.resolve(Const.XML));
        logger.finest("\n" + UTF8Codec.toString(xmlUgly));
        Assert.assertEquals(283, xmlUgly.length);
        Assert.assertEquals("96fe8f4d", CRCU.crc32String(xmlUgly));
        // this transform pretty prints (80 columns max, no normalize of element xmlns attributes)
        final Document document = DocumentU.toDocument(xmlUgly);
        final byte[] xmlNew = toXmlPrettyLS(document);
        logger.finest("\n" + UTF8Codec.toString(xmlNew));
        if (SystemU.javaVersion().startsWith("1.8")) {
            Assert.assertEquals(270, xmlNew.length);
            Assert.assertEquals("8b6a8189", CRCU.crc32String(xmlNew));
        }
    }

    private byte[] toXmlPrettyLS(final Document document) throws IOException {
        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DOMImplementationRegistry registry = DOMImplementationRegistry.newInstance();
            DOMImplementationLS loadSave = (DOMImplementationLS) registry.getDOMImplementation("LS");
            LSOutput output = loadSave.createLSOutput();
            output.setByteStream(bos);
            LSSerializer serializer = loadSave.createLSSerializer();
            DOMConfiguration config = serializer.getDomConfig();
            DOMStringList parameterNames = config.getParameterNames();
            int length = parameterNames.getLength();
            for (int i = 0; (i < length); ++i) {
                String parameterName = parameterNames.item(i);
                logger.finest(parameterName + "/" + config.canSetParameter(parameterName, true));
/*
namespaces/true
split-cdata-sections/true
discard-default-content/true
xml-declaration/true
canonical-form/false
validate-if-schema/false
validate/false
check-character-normalization/false
datatype-normalization/false
format-pretty-print/true
well-formed/true
infoset/true
namespace-declarations/true
element-content-whitespace/true
entities/true
cdata-sections/true
comments/true
ignore-unknown-character-denormalizations/true
error-handler/false
*/
            }
            config.setParameter("format-pretty-print", true);
            serializer.write(document, output);
            return bos.toByteArray();
        } catch (Exception e) {
            throw new IOException(e.getMessage());
        }
    }

    public static class Const {
        public static final String XML = "io/github/greyp9/arwo/xml/ugly/ugly.xml";
    }
}
