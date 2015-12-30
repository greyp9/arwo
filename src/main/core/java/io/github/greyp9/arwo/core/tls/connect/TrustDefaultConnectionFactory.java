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

    public TrustDefaultConnectionFactory(String protocol) throws GeneralSecurityException {
        SSLContext context = SSLContext.getInstance(protocol);
        context.init(null, null, null);
        socketFactory = context.getSocketFactory();
    }

    public HttpsURLConnection openConnection(URL url) throws IOException {
        return openConnection(url.openConnection());
    }

    private HttpsURLConnection openConnection(URLConnection urlConnection) {
        HttpsURLConnection httpsURLConnection = (HttpsURLConnection) urlConnection;
        httpsURLConnection.setSSLSocketFactory(socketFactory);
        return httpsURLConnection;
    }
}
