package io.github.greyp9.arwo.core.tls.connect;

import io.github.greyp9.arwo.core.tls.context.TLSContext;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSocketFactory;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.security.GeneralSecurityException;

public class TrustExplicitConnectionFactory {
    private final SSLSocketFactory socketFactory;

    public TrustExplicitConnectionFactory(TLSContext context) throws GeneralSecurityException {
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
