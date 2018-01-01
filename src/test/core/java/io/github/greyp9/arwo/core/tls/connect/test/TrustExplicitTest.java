package io.github.greyp9.arwo.core.tls.connect.test;

import io.github.greyp9.arwo.core.tls.client.CertificateClient;
import io.github.greyp9.arwo.core.tls.connect.TrustExplicitConnectionFactory;
import io.github.greyp9.arwo.core.tls.context.TLSContext;
import io.github.greyp9.arwo.core.tls.context.TLSContextFactory;
import org.junit.Assert;
import org.junit.Test;

import javax.net.ssl.HttpsURLConnection;
import java.io.IOException;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.logging.Logger;

public class TrustExplicitTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    /**
     * Test access to custom SSL context, which redefines the trusted server certificate.
     */
    @Test
    public void testExplicitTrust() throws GeneralSecurityException, IOException {
        // acquire trusted certificate
        final URL url = new URL("https://localhost:8443/");
        logger.finest(url.toExternalForm());
        final CertificateClient client = new CertificateClient("TLS");
        final Collection<X509Certificate> certificates = client.getCertificateChain(url);
        final X509Certificate certificate = certificates.iterator().next();
        logger.finest(certificate.getSubjectDN().getName());
        // perform server request
        final TLSContext context = new TLSContextFactory().create(certificate, "TLS");
        final TrustExplicitConnectionFactory connectionFactory = new TrustExplicitConnectionFactory(context);
        final HttpsURLConnection urlConnection = connectionFactory.openConnection(url);
        Assert.assertNotNull(urlConnection);
        urlConnection.connect();
        urlConnection.disconnect();
    }
}
