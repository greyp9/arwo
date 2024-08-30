package io.github.greyp9.arwo.core.tls.connect;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import java.io.IOException;
import java.net.Proxy;
import java.net.URL;
import java.net.URLConnection;
import java.security.GeneralSecurityException;

public final class TrustDefaultConnectionFactory {
    private final SSLSocketFactory socketFactory;

    public TrustDefaultConnectionFactory(final String protocol) throws GeneralSecurityException {
        final SSLContext context = SSLContext.getInstance(protocol);
        context.init(null, null, null);
        socketFactory = context.getSocketFactory();
    }

    public HttpsURLConnection openConnection(final URL url) throws IOException {
        return openConnection(url.openConnection());
    }

    public HttpsURLConnection openConnection(final URL url, final Proxy proxy) throws IOException {
        return openConnection(url.openConnection(proxy));
    }

    private HttpsURLConnection openConnection(final URLConnection urlConnection) {
        final HttpsURLConnection httpsConnection = (HttpsURLConnection) urlConnection;
        httpsConnection.setSSLSocketFactory(socketFactory);
        return httpsConnection;
    }
}
