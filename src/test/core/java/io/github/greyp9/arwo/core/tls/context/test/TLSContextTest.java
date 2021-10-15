package io.github.greyp9.arwo.core.tls.context.test;

import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.httpclient.HttpsClient;
import io.github.greyp9.arwo.core.tls.client.CertificateClient;
import io.github.greyp9.arwo.core.value.NTV;
import org.junit.Assert;
import org.junit.Test;

import javax.net.ssl.SSLHandshakeException;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.logging.Logger;

/**
 * Tests for default / custom TLS contexts.
 */
public class TLSContextTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    /**
     * Test access to default SSL context, which uses built in trust store.
     */
    @Test
    public void testDefaultTrust() throws IOException, GeneralSecurityException {
        final URL url = new URL("https://www.google.com/");
        logger.finest(url.toExternalForm());
        final HttpsClient httpsClient = new HttpsClient(null, true);
        final HttpRequest httpRequest = new HttpRequest(Http.Method.GET, "/", null, NTV.create(), null);
        final HttpResponse httpResponse = httpsClient.doRequest(url, httpRequest);
        Assert.assertEquals(HttpURLConnection.HTTP_OK, httpResponse.getStatusCode());
    }

    /**
     * Test failure to connect to server using self signed certificate.
     */
    @Test
    public void testDefaultTrustNegative() throws IOException, GeneralSecurityException {
        final URL url = new URL("https://localhost:8443/");
        logger.finest(url.toExternalForm());
        final HttpsClient httpsClient = new HttpsClient(null, true);
        final HttpRequest httpRequest = new HttpRequest(Http.Method.GET, "/", null, NTV.create(), null);
        try {
            httpsClient.doRequest(url, httpRequest);
            Assert.fail("SSLHandshakeException / ValidatorException expected");
        } catch (SSLHandshakeException e) {
            logger.severe(e.getMessage());
            //Assert.assertTrue(e.getMessage().contains("ValidatorException"));  // updated message
        } catch (Exception e) {
            logger.severe(e.getMessage());
            Assert.fail("SSLHandshakeException / ValidatorException expected");
        }
    }

    /**
     * Test access to custom SSL context, which redefines the trusted server certificate.
     */
    @Test
    public void testExplicitTrust() throws GeneralSecurityException, IOException {
        // acquire trusted certificate
        final URL url = new URL("https://localhost:8443/");
        final CertificateClient client = new CertificateClient("TLS");
        final Collection<X509Certificate> certificates = client.getCertificateChain(url);
        final X509Certificate certificate = certificates.iterator().next();
        logger.finest(certificate.getSubjectDN().getName());
        // perform server request
        final HttpsClient httpsClient = new HttpsClient(certificate, false);
        final HttpRequest httpRequest = new HttpRequest(Http.Method.GET, "/", null, NTV.create(), null);
        final HttpResponse httpResponse = httpsClient.doRequest(url, httpRequest);
        Assert.assertTrue(httpResponse.getStatusCode() >= HttpURLConnection.HTTP_MULT_CHOICE);
        // (server configuration)
        //Assert.assertEquals(HttpURLConnection.HTTP_UNAUTHORIZED, httpResponse.getStatusCode());
    }

    /**
     * Test usage of custom SSL context, where accessed server does not present expected certificate.
     */
    @Test
    public void testExplicitTrustNegative() throws GeneralSecurityException, IOException {
        // acquire trusted certificate
        final URL urlTrust = new URL("https://localhost:8443/");
        final CertificateClient client = new CertificateClient("TLS");
        final Collection<X509Certificate> certificates = client.getCertificateChain(urlTrust);
        final X509Certificate certificate = certificates.iterator().next();
        logger.finest(certificate.getSubjectDN().getName());
        // perform server request
        final URL url = new URL("https://www.google.com/");
        logger.finest(url.toExternalForm());
        final HttpsClient httpsClient = new HttpsClient(certificate, false);
        final HttpRequest httpRequest = new HttpRequest(Http.Method.GET, "/", null, NTV.create(), null);
        try {
            httpsClient.doRequest(url, httpRequest);
            Assert.fail("SSLHandshakeException / ValidatorException expected");
        } catch (SSLHandshakeException e) {
            logger.severe(e.getMessage());
            //Assert.assertTrue(e.getMessage().contains("ValidatorException"));  // updated message
        } catch (Exception e) {
            Assert.fail("SSLHandshakeException / ValidatorException expected");
        }
    }
}
