package io.github.greyp9.arwo.core.tls.client;

import io.github.greyp9.arwo.core.tls.connect.TrustAllConnectionFactory;
import io.github.greyp9.arwo.core.tls.context.TLSContext;
import io.github.greyp9.arwo.core.tls.context.TLSContextFactory;

import javax.net.ssl.HttpsURLConnection;
import java.io.IOException;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;

public class CertificateClient {
    private final String protocol;

    public CertificateClient(final String protocol) {
        this.protocol = protocol;
    }

    public final Collection<X509Certificate> getCertificateChain(final URL url)
            throws GeneralSecurityException, IOException {
        final Collection<X509Certificate> certificatesX509 = new ArrayList<X509Certificate>();
        final TLSContext context = new TLSContextFactory().createTrustAll(protocol);
        final TrustAllConnectionFactory connectionFactory = new TrustAllConnectionFactory(context);
        final HttpsURLConnection connection = connectionFactory.openConnection(url);
        connection.connect();
        final Certificate[] certificates = connection.getServerCertificates();
        connection.disconnect();
        for (final Certificate certificate : certificates) {
            if (certificate instanceof X509Certificate) {
                certificatesX509.add((X509Certificate) certificate);
            }
        }
        return certificatesX509;
    }
}
