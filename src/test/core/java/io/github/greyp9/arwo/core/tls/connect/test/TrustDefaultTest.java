package io.github.greyp9.arwo.core.tls.connect.test;

import io.github.greyp9.arwo.core.tls.connect.TrustDefaultConnectionFactory;
import org.junit.Assert;
import org.junit.Test;

import javax.net.ssl.HttpsURLConnection;
import java.io.IOException;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.util.logging.Logger;

public class TrustDefaultTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testDefaultTrust() throws GeneralSecurityException, IOException {
        final URL url = new URL("https://www.google.com/");
        logger.finest(url.toExternalForm());
        // perform server request
        final TrustDefaultConnectionFactory connectionFactory = new TrustDefaultConnectionFactory("TLS");
        final HttpsURLConnection urlConnection = connectionFactory.openConnection(url);
        Assert.assertNotNull(urlConnection);
        urlConnection.connect();
        urlConnection.disconnect();
    }
}
