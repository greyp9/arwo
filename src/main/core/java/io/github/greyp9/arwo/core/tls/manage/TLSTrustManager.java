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

    public TLSTrustManager(X509Certificate[] certificates) {
        this(certificates, TrustManagerFactory.getDefaultAlgorithm());
    }

    public TLSTrustManager(X509Certificate[] certificates, String algorithm) {
        this.certificates = certificates;
        this.algorithm = algorithm;
    }

    public TrustManager[] getTrustManagers() throws GeneralSecurityException {
        return (certificates == null) ? getTrustManagersN() : getTrustManagersNN();
    }

    private TrustManager[] getTrustManagersN() throws GeneralSecurityException {
        return new TrustManager[] { new TrustAllTrustManager() };
    }

    private TrustManager[] getTrustManagersNN() throws GeneralSecurityException {
        KeyStore keyStore = createKeyStore();
        TrustManagerFactory factory = TrustManagerFactory.getInstance(algorithm);
        factory.init(keyStore);
        return factory.getTrustManagers();
    }

    private KeyStore createKeyStore() throws GeneralSecurityException {
        KeyStore keyStore = null;
        if (certificates != null) {
            keyStore = createEmptyKeyStore();
            for (X509Certificate certificate : certificates) {
                keyStore.setCertificateEntry(certificate.getSubjectDN().getName(), certificate);
            }
        }
        return keyStore;
    }

    private KeyStore createEmptyKeyStore() throws GeneralSecurityException {
        KeyStore keyStore = KeyStore.getInstance(KeyStore.getDefaultType());
        try {
            keyStore.load(null, null);
        } catch (IOException e) {
            throw new GeneralSecurityException(e);
        }
        return keyStore;
    }
}
