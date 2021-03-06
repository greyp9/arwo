package io.github.greyp9.arwo.core.tls.context;

import io.github.greyp9.arwo.core.cer.CertificateU;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.tls.manage.TLSTrustManager;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.cert.X509Certificate;

public class TLSContextFactory {

    public final TLSContext create(final String pemCertificate, final String protocol) throws IOException {
        return create(UTF8Codec.toBytes(pemCertificate), protocol);
    }

    public final TLSContext create(final byte[] bytesCertificate, final String protocol) throws IOException {
        try {
            return create(CertificateU.toX509(bytesCertificate), protocol);
        } catch (GeneralSecurityException e) {
            throw new IOException(e);
        }
    }

    public final TLSContext create(
            final X509Certificate certificate, final String protocol) throws GeneralSecurityException {
        final TLSTrustManager trustManager = new TLSTrustManager(new X509Certificate[] { certificate });
        return new TLSContext(null, trustManager, protocol);
    }

    public final TLSContext createTrustAll(final String protocol) throws GeneralSecurityException {
        final TLSTrustManager trustManager = new TLSTrustManager((KeyStore) null);
        return new TLSContext(null, trustManager, protocol);
    }
}
