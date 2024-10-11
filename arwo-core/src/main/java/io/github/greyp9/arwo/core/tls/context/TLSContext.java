package io.github.greyp9.arwo.core.tls.context;

import io.github.greyp9.arwo.core.tls.manage.TLSKeyManager;
import io.github.greyp9.arwo.core.tls.manage.TLSTrustManager;

import javax.net.ssl.KeyManager;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLServerSocketFactory;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import java.security.GeneralSecurityException;

public class TLSContext {
    private final TLSKeyManager tlsKeyManager;
    private final TLSTrustManager tlsTrustManager;
    private final String protocol;

    public TLSContext(final TLSKeyManager tlsKeyManager, final TLSTrustManager tlsTrustManager, final String protocol) {
        this.tlsKeyManager = tlsKeyManager;
        this.tlsTrustManager = tlsTrustManager;
        // SSLv2, SSLv3, TLS, TLSv1.1, TLSv1.2
        this.protocol = protocol;
    }

    public final X509TrustManager getTrustManager() throws GeneralSecurityException {
        final TrustManager[] trustManagers = tlsTrustManager.createTrustManagers();
        final TrustManager trustManager = (trustManagers.length == 0) ? null : trustManagers[0];
        return (trustManager instanceof X509TrustManager) ? (X509TrustManager) trustManager : null;
    }

    public final String getProtocol() {
        return protocol;
    }

    public final SSLSocketFactory getSocketFactory() throws GeneralSecurityException {
        final SSLContext context = getContext();
        return context.getSocketFactory();
    }

    public final SSLServerSocketFactory getServerSocketFactory() throws GeneralSecurityException {
        final SSLContext context = getContext();
        return context.getServerSocketFactory();
    }

    public final SSLContext getContext() throws GeneralSecurityException {
        final SSLContext context = SSLContext.getInstance(protocol);
        final KeyManager[] keyManagers = tlsKeyManager == null ? null : tlsKeyManager.createKeyManagers();
        final TrustManager[] trustManagers = tlsTrustManager == null ? null : tlsTrustManager.createTrustManagers();
        context.init(keyManagers, trustManagers, null);
        return context;
    }
}
