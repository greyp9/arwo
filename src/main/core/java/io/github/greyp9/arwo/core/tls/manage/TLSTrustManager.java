package io.github.greyp9.arwo.core.tls.manage;

import io.github.greyp9.arwo.core.tls.trust.TrustAllTrustManager;

import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.cert.X509Certificate;

public class TLSTrustManager {
    private final X509Certificate[] certificates;
    private final String algorithm;

    @SuppressWarnings("PMD.UseVarargs")
    public TLSTrustManager(final X509Certificate[] certificates) {
        this(certificates, TrustManagerFactory.getDefaultAlgorithm());
    }

    public TLSTrustManager(final X509Certificate[] certificates, final String algorithm) {
        this.certificates = ((certificates == null) ? null : certificates.clone());
        this.algorithm = algorithm;
    }

    @SuppressWarnings("PMD.MethodReturnsInternalArray")
    public final TrustManager[] createTrustManagers() throws GeneralSecurityException {
        return (certificates == null) ? createTrustManagersN() : createTrustManagersNN();
    }

    private TrustManager[] createTrustManagersN() throws GeneralSecurityException {
        return new TrustManager[] { new TrustAllTrustManager() };
    }

    private TrustManager[] createTrustManagersNN() throws GeneralSecurityException {
        final KeyStore keyStore = createKeyStore();
        final TrustManagerFactory factory = TrustManagerFactory.getInstance(algorithm);
        factory.init(keyStore);
        return factory.getTrustManagers();
    }

    private KeyStore createKeyStore() throws GeneralSecurityException {
        KeyStore keyStore = null;
        if (certificates != null) {
            keyStore = createEmptyKeyStore();
            for (final X509Certificate certificate : certificates) {
                keyStore.setCertificateEntry(certificate.getSubjectDN().getName(), certificate);
            }
        }
        return keyStore;
    }

    private KeyStore createEmptyKeyStore() throws GeneralSecurityException {
        final KeyStore keyStore = KeyStore.getInstance(KeyStore.getDefaultType());
        try {
            keyStore.load(null, null);
        } catch (IOException e) {
            throw new GeneralSecurityException(e);
        }
        return keyStore;
    }
}
