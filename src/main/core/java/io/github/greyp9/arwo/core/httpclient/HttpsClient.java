package io.github.greyp9.arwo.core.httpclient;

import io.github.greyp9.arwo.core.http.HttpRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.tls.connect.TrustExplicitConnectionFactory;
import io.github.greyp9.arwo.core.tls.context.TLSContext;
import io.github.greyp9.arwo.core.tls.context.TLSContextFactory;
import io.github.greyp9.arwo.core.tls.verifier.TrustAllHostnameVerifier;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import java.io.IOException;
import java.net.Proxy;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.security.cert.X509Certificate;

public class HttpsClient extends HttpClient {
    private final TrustExplicitConnectionFactory connectionFactory;
    private final HostnameVerifier hostnameVerifier;

    public HttpsClient(final X509Certificate certificate, final boolean verifyHostnames)
            throws GeneralSecurityException {
        this(certificate, verifyHostnames, null);
    }

    public HttpsClient(final X509Certificate certificate, final boolean verifyHostnames, final Proxy proxy)
            throws GeneralSecurityException {
        super(proxy);
        final TLSContext context = ((certificate == null) ?
                new TLSContext(null, null, "TLS") :  // default trust store
                new TLSContextFactory().create(certificate, "TLS"));  // custom trust store
        connectionFactory = new TrustExplicitConnectionFactory(context);
        hostnameVerifier = (verifyHostnames ? null : new TrustAllHostnameVerifier());
    }

    protected HttpsURLConnection getConnection(final URL url) throws IOException {
        final Proxy proxy = getProxy();
        return ((proxy == null) ?
                connectionFactory.openConnection(url) : connectionFactory.openConnection(url, proxy));
    }

    public final HttpResponse doRequest(
            final URL url, final HttpRequest httpRequest) throws IOException {
        final URL urlRequest = toURL(url, httpRequest);
        final HttpsURLConnection connection = getConnection(urlRequest);
        if (hostnameVerifier != null) {
            connection.setHostnameVerifier(hostnameVerifier);
        }
        return doRequest(connection, httpRequest);
    }
}
