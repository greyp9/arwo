package io.github.greyp9.arwo.core.tls.context;

import io.github.greyp9.arwo.core.tls.manage.TLSKeyManager;
import io.github.greyp9.arwo.core.tls.manage.TLSTrustManager;

import javax.net.ssl.KeyManager;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import java.security.GeneralSecurityException;

public class TLSContext {
    private final TLSKeyManager tlsKeyManager;
    private final TLSTrustManager tlsTrustManager;
    private final String protocol;

    public TLSContext(TLSKeyManager tlsKeyManager, TLSTrustManager tlsTrustManager, String protocol) {
        this.tlsKeyManager = tlsKeyManager;
        this.tlsTrustManager = tlsTrustManager;
        // SSLv2, SSLv3, TLS, TLSv1.1, TLSv1.2
        this.protocol = protocol;
    }

    public String getProtocol() {
        return protocol;
    }

    public SSLSocketFactory getSocketFactory() throws GeneralSecurityException {
        SSLContext context = getContext();
        return context.getSocketFactory();
    }

/*
    public SSLServerSocketFactory getServerSocketFactory() throws GeneralSecurityException {
        SSLContext context = getContext();
        return context.getServerSocketFactory();
    }
*/

    public SSLContext getContext() throws GeneralSecurityException {
        SSLContext context = SSLContext.getInstance(protocol);
        KeyManager[] keyManagers = tlsKeyManager == null ? null : tlsKeyManager.getKeyManagers();
        TrustManager[] trustManagers = tlsTrustManager == null ? null : tlsTrustManager.getTrustManagers();
        context.init(keyManagers, trustManagers, null);
        return context;
    }
}
