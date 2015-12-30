package io.github.greyp9.arwo.core.tls.connect;

import io.github.greyp9.arwo.core.tls.context.TLSContext;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSocketFactory;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.security.GeneralSecurityException;

public class TrustAllConnectionFactory {
    private final SSLSocketFactory socketFactory;

    public TrustAllConnectionFactory(final TLSContext context) throws GeneralSecurityException {
        socketFactory = context.getSocketFactory();
    }

    public final HttpsURLConnection openConnection(final URL url) throws IOException {
        return openConnection(url.openConnection());
    }

    private HttpsURLConnection openConnection(final URLConnection urlConnection) {
        final HttpsURLConnection httpsConnection = (HttpsURLConnection) urlConnection;
        httpsConnection.setSSLSocketFactory(socketFactory);
        return httpsConnection;
    }
}
