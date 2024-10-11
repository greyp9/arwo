package io.github.greyp9.arwo.core.cer.test;

import io.github.greyp9.arwo.core.cer.CertificateU;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.tls.client.CertificateClient;
import io.github.greyp9.arwo.core.tls.verifier.TrustAllHostnameVerifier;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.net.URL;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.logging.Logger;

public class CertificateAnyHostnameTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    @Disabled("live network access")
    public void testFetchCertificate() throws Exception {
        final URL url = new URL("https://localhost:7443");
        final CertificateClient client = new CertificateClient("TLSv1.2", new TrustAllHostnameVerifier());
        final Collection<X509Certificate> certificates = client.getCertificateChain(url);
        final X509Certificate certificate = certificates.iterator().next();
        logger.info(UTF8Codec.toString(CertificateU.toBytes(certificate)));
    }
}
