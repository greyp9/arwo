package io.github.greyp9.arwo.core.tls.manage;

import io.github.greyp9.arwo.core.lang.CharU;
import io.github.greyp9.arwo.core.value.Value;

import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collections;

public class TLSKeyManager {
    private final KeyStore keyStore;
    private final char[] passphrase;
    private final String algorithm;

    @SuppressWarnings("PMD.UseVarargs")
    public TLSKeyManager(final KeyStore keyStore, final char[] passphrase) {
        this(keyStore, passphrase, KeyManagerFactory.getDefaultAlgorithm());
    }

    public TLSKeyManager(final KeyStore keyStore, final char[] passphrase, final String algorithm) {
        this.keyStore = keyStore;
        this.passphrase = CharU.copy(passphrase);
        this.algorithm = algorithm;
    }

    public final X509Certificate getCertificate() throws KeyStoreException {
        final ArrayList<String> aliases = Collections.list(keyStore.aliases());
        final String alias = (aliases.isEmpty() ? null : aliases.iterator().next());
        return (alias == null) ? null : getCertificate(alias);
    }

    public final X509Certificate getCertificate(final String alias) throws KeyStoreException {
        return Value.as(keyStore.getCertificate(alias), X509Certificate.class);
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
