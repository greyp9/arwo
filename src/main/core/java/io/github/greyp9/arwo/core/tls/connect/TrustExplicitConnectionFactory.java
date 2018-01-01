package io.github.greyp9.arwo.core.tls.connect;

import io.github.greyp9.arwo.core.tls.context.TLSContext;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSocketFactory;
import java.io.IOException;
import java.net.Proxy;
import java.net.URL;
import java.net.URLConnection;
import java.security.GeneralSecurityException;

public class TrustExplicitConnectionFactory {
    private final SSLSocketFactory socketFactory;

    public TrustExplicitConnectionFactory(final TLSContext context) throws GeneralSecurityException {
        socketFactory = context.getSocketFactory();
    }

    public final HttpsURLConnection openConnection(final URL url) throws IOException {
        return openConnection(url.openConnection());
    }

    public final HttpsURLConnection openConnection(final URL url, final Proxy proxy) throws IOException {
        return openConnection(url.openConnection(proxy));
    }

    private HttpsURLConnection openConnection(final URLConnection urlConnection) {
        final HttpsURLConnection httpsConnection = (HttpsURLConnection) urlConnection;
        httpsConnection.setSSLSocketFactory(socketFactory);
        return httpsConnection;
    }
}
