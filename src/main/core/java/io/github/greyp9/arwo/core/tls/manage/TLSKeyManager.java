package io.github.greyp9.arwo.core.tls.manage;

import io.github.greyp9.arwo.core.lang.CharU;

import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import java.security.GeneralSecurityException;
import java.security.KeyStore;

public class TLSKeyManager {
    private final KeyStore keyStore;
    private final char[] passphrase;
    private final String algorithm;

/*
    public TLSKeyManager(KeyStore keyStore, char[] passphrase) {
        this(keyStore, passphrase, KeyManagerFactory.getDefaultAlgorithm());
    }
*/

    public TLSKeyManager(final KeyStore keyStore, final char[] passphrase, final String algorithm) {
        this.keyStore = keyStore;
        this.passphrase = CharU.copy(passphrase);
        this.algorithm = algorithm;
    }

    @SuppressWarnings("PMD.MethodReturnsInternalArray")
    public final KeyManager[] createKeyManagers() throws GeneralSecurityException {
        return (keyStore == null) ? null : createKeyManagersNN();
    }

    private KeyManager[] createKeyManagersNN() throws GeneralSecurityException {
        final KeyManagerFactory factory = KeyManagerFactory.getInstance(algorithm);
        factory.init(keyStore, passphrase);
        return factory.getKeyManagers();
    }
}
