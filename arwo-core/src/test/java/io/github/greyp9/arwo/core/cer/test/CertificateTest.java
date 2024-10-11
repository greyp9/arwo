package io.github.greyp9.arwo.core.cer.test;

import io.github.greyp9.arwo.core.cer.CertificateU;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.security.cert.X509Certificate;
import java.util.logging.Logger;

public class CertificateTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testCodec() throws Exception {
        for (String resource : Const.RESOURCES) {
            final byte[] bytesOriginal = StreamU.read(ResourceU.resolve(resource));
            Assertions.assertNotNull(bytesOriginal);
            logger.finest(UTF8Codec.toString(bytesOriginal));
            final X509Certificate certificate = CertificateU.toX509(bytesOriginal);
            logger.finest(certificate.getSubjectDN().toString());
            final byte[] bytesCopy = CertificateU.toBytes(certificate);
            logger.finest(UTF8Codec.toString(bytesCopy));
            Assertions.assertEquals(CRCU.crc32(bytesOriginal), CRCU.crc32(bytesCopy));
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
