package io.github.greyp9.arwo.core.tls.manage;

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

    public TLSKeyManager(KeyStore keyStore, char[] passphrase, String algorithm) {
        this.keyStore = keyStore;
        this.passphrase = passphrase;
        this.algorithm = algorithm;
    }

    public KeyManager[] getKeyManagers() throws GeneralSecurityException {
        return (keyStore == null) ? null : getKeyManagersNN();
    }

    private KeyManager[] getKeyManagersNN() throws GeneralSecurityException {
        KeyManagerFactory factory = KeyManagerFactory.getInstance(algorithm);
        factory.init(keyStore, passphrase);
        return factory.getKeyManagers();
    }
}
