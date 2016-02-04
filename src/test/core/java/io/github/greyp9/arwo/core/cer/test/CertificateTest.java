package io.github.greyp9.arwo.core.cer.test;

import io.github.greyp9.arwo.core.cer.CertificateU;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import junit.framework.TestCase;
import org.junit.Assert;

import java.security.cert.X509Certificate;
import java.util.logging.Logger;

public class CertificateTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public void testCodec() throws Exception {
        for (String resource : Const.RESOURCES) {
            final byte[] bytesOriginal = StreamU.read(ResourceU.resolve(resource));
            Assert.assertNotNull(bytesOriginal);
            logger.info(UTF8Codec.toString(bytesOriginal));
            final X509Certificate certificate = CertificateU.toX509(bytesOriginal);
            logger.info(certificate.getSubjectDN().toString());
            final byte[] bytesCopy = CertificateU.toBytes(certificate);
            logger.info(UTF8Codec.toString(bytesCopy));
            Assert.assertEquals(CRCU.crc32(bytesOriginal), CRCU.crc32(bytesCopy));
        }
    }

    private static class Const {
        private static final String[] RESOURCES = {
                "io/github/greyp9/arwo/cer/1a.cer",
                "io/github/greyp9/arwo/cer/1b.cer",
                "io/github/greyp9/arwo/cer/1c.cer",
        };
    }
}
