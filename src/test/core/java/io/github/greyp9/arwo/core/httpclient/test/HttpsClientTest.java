package io.github.greyp9.arwo.core.httpclient.test;

import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.httpclient.HttpClientU;
import io.github.greyp9.arwo.core.httpclient.HttpsClient;
import io.github.greyp9.arwo.core.tls.client.CertificateClient;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.net.HttpURLConnection;
import java.net.URL;
import java.security.cert.X509Certificate;
import java.util.Collection;

public class HttpsClientTest {
    //private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    @Ignore
    public void testHttpsGet() throws Exception {
        final URL url = new URL("https://localhost:8443/");
        final CertificateClient client = new CertificateClient("TLS");
        final Collection<X509Certificate> certificates = client.getCertificateChain(url);
        final X509Certificate certificate = certificates.iterator().next();
        final NameTypeValues headersRequest = new NameTypeValues();
        final HttpRequest httpRequest = new HttpRequest(Http.Method.GET, "/", null, headersRequest, null);
        final HttpsClient httpsClient = new HttpsClient(certificate, false);
        final HttpResponse httpResponse = httpsClient.doRequest(url, httpRequest);
        Assert.assertTrue(httpResponse.getStatusCode() >= HttpURLConnection.HTTP_MULT_CHOICE);
        final NameTypeValues headersResponse = httpResponse.getHeaders();
        final Collection<String> valuesAuthenticate = headersResponse.getValues(Http.Header.WWW_AUTHENTICATE);
        Assert.assertFalse(valuesAuthenticate.isEmpty());
        Assert.assertEquals(1, valuesAuthenticate.size());
        Assert.assertTrue(valuesAuthenticate.iterator().next().contains(Http.Realm.BASIC));
    }

    @Test
    @Ignore
    public void testHttpsGetAuth() throws Exception {
        final URL url = new URL("https://localhost:8443/");
        final CertificateClient client = new CertificateClient("TLS");
        final Collection<X509Certificate> certificates = client.getCertificateChain(url);
        final X509Certificate certificate = certificates.iterator().next();
        final String basicAuth = HttpClientU.toBasicAuth("arwo", "arwo".toCharArray());
        final NameTypeValues headersRequest = NTV.create(Http.Header.AUTHORIZATION, basicAuth);
        final HttpRequest httpRequest = new HttpRequest(Http.Method.GET, "/", null, headersRequest, null);
        final HttpsClient httpsClient = new HttpsClient(certificate, false);
        final HttpResponse httpResponse = httpsClient.doRequest(url, httpRequest);
        Assert.assertEquals(HttpURLConnection.HTTP_OK, httpResponse.getStatusCode());
        final NameTypeValues headersResponse = httpResponse.getHeaders();
        final Collection<String> valuesContentType = headersResponse.getValues(Http.Header.CONTENT_TYPE);
        Assert.assertFalse(valuesContentType.isEmpty());
        Assert.assertEquals(1, valuesContentType.size());
        Assert.assertTrue(valuesContentType.iterator().next().contains(Http.Mime.TEXT_HTML_UTF8));
    }

    @Test
    public void testBasicAuthorization() throws Exception {
        final String basicAuth = HttpClientU.toBasicAuth("arwo", "arwo".toCharArray());
        Assert.assertEquals("Basic YXJ3bzphcndv", basicAuth);

    }
}
