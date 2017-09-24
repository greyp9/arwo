package io.github.greyp9.arwo.core.httpclient.test;

import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.httpclient.HttpsClient;
import io.github.greyp9.arwo.core.tls.client.CertificateClient;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import junit.framework.TestCase;
import org.junit.Assert;

import java.net.HttpURLConnection;
import java.net.URL;
import java.security.cert.X509Certificate;
import java.util.Collection;

public class HttpsClientTest extends TestCase {
    //private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testHttpGet() throws Exception {
        final URL url = new URL("https://localhost:8443/");
        final CertificateClient client = new CertificateClient("TLS");
        final Collection<X509Certificate> certificates = client.getCertificateChain(url);
        final X509Certificate certificate = certificates.iterator().next();
        final NameTypeValues headersRequest = new NameTypeValues();
        final HttpRequest httpRequest = new HttpRequest(Http.Method.GET, "/", null, headersRequest, null);
        final HttpsClient httpsClient = new HttpsClient(certificate, false);
        final HttpResponse httpResponse = httpsClient.doRequest(url, httpRequest);
        Assert.assertEquals(HttpURLConnection.HTTP_UNAUTHORIZED, httpResponse.getStatusCode());
        final NameTypeValues headersResponse = httpResponse.getHeaders();
        final Collection<String> valuesAuthenticate = headersResponse.getValues(Http.Header.WWW_AUTHENTICATE);
        Assert.assertFalse(valuesAuthenticate.isEmpty());
        Assert.assertEquals(1, valuesAuthenticate.size());
        Assert.assertTrue(valuesAuthenticate.iterator().next().contains(Http.Realm.BASIC));
    }
}
