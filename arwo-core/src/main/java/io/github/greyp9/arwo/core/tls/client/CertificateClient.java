package io.github.greyp9.arwo.core.tls.client;

import io.github.greyp9.arwo.core.tls.connect.TrustAllConnectionFactory;
import io.github.greyp9.arwo.core.tls.context.TLSContext;
import io.github.greyp9.arwo.core.tls.context.TLSContextFactory;
import io.github.greyp9.arwo.core.tls.manage.TLSKeyManager;

import javax.net.ssl.HostnameVerifier;
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
    private final HostnameVerifier hostnameVerifier;

    public CertificateClient(final String protocol) {
        this(protocol, null);
    }

    public CertificateClient(final String protocol, final HostnameVerifier hostnameVerifier) {
        this.protocol = protocol;
        this.hostnameVerifier = hostnameVerifier;
    }

    public final Collection<X509Certificate> getCertificateChain(final URL url)
            throws GeneralSecurityException, IOException {
        return getCertificateChain(url, null);
    }

    public final Collection<X509Certificate> getCertificateChain(final URL url, final TLSKeyManager keyManager)
            throws GeneralSecurityException, IOException {
        final Collection<X509Certificate> certificatesX509 = new ArrayList<X509Certificate>();
        final TLSContext context = new TLSContextFactory().createTrustAll(protocol, keyManager);
        final TrustAllConnectionFactory connectionFactory = new TrustAllConnectionFactory(context);
        final HttpsURLConnection connection = connectionFactory.openConnection(url);
        if (hostnameVerifier != null) {
            connection.setHostnameVerifier(hostnameVerifier);
        }
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
