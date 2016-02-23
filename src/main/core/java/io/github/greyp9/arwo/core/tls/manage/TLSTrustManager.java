package io.github.greyp9.arwo.core.tls.manage;

import io.github.greyp9.arwo.core.tls.trust.TrustAllTrustManager;

import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.cert.X509Certificate;

public class TLSTrustManager {
    private final KeyStore keyStore;
    private final String algorithm;

    public TLSTrustManager(final KeyStore keyStore) {
        this.keyStore = keyStore;
        this.algorithm = TrustManagerFactory.getDefaultAlgorithm();
    }

    @SuppressWarnings("PMD.UseVarargs")
    public TLSTrustManager(final X509Certificate[] certificates) throws GeneralSecurityException {
        this(certificates, TrustManagerFactory.getDefaultAlgorithm());
    }

    public TLSTrustManager(
            final X509Certificate[] certificates, final String algorithm) throws GeneralSecurityException {
        this.keyStore = createKeyStore(certificates);
        this.algorithm = algorithm;
    }

    @SuppressWarnings("PMD.MethodReturnsInternalArray")
    public final TrustManager[] createTrustManagers() throws GeneralSecurityException {
        return (keyStore == null) ? createTrustManagersN() : createTrustManagersNN();
    }

    private TrustManager[] createTrustManagersN() throws GeneralSecurityException {
        return new TrustManager[] { new TrustAllTrustManager() };
    }

    private TrustManager[] createTrustManagersNN() throws GeneralSecurityException {
        final TrustManagerFactory factory = TrustManagerFactory.getInstance(algorithm);
        factory.init(keyStore);
        return factory.getTrustManagers();
    }

    private static KeyStore createKeyStore(final X509Certificate[] certificates) throws GeneralSecurityException {
        KeyStore keyStore = null;
        if (certificates != null) {
            keyStore = createEmptyKeyStore();
            for (final X509Certificate certificate : certificates) {
                keyStore.setCertificateEntry(certificate.getSubjectDN().getName(), certificate);
            }
        }
        return keyStore;
    }

    private static KeyStore createEmptyKeyStore() throws GeneralSecurityException {
        final KeyStore keyStore = KeyStore.getInstance(KeyStore.getDefaultType());
        try {
            keyStore.load(null, null);
        } catch (IOException e) {
            throw new GeneralSecurityException(e);
        }
        return keyStore;
    }
}
