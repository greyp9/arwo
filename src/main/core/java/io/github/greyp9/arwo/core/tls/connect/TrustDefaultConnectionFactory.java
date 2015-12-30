package io.github.greyp9.arwo.core.tls.connect;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.security.GeneralSecurityException;

public class TrustDefaultConnectionFactory {
    private final SSLSocketFactory socketFactory;

    public TrustDefaultConnectionFactory(final String protocol) throws GeneralSecurityException {
        final SSLContext context = SSLContext.getInstance(protocol);
        context.init(null, null, null);
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
