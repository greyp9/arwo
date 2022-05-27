package io.github.greyp9.arwo.core.tls.context;

import io.github.greyp9.arwo.core.cer.CertificateU;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.tls.manage.TLSKeyManager;
import io.github.greyp9.arwo.core.tls.manage.TLSTrustManager;

import java.io.ByteArrayInputStream;
import java.io.File;
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

    public final TLSKeyManager getKeyManager(final String type, final String path, final char[] password)
            throws GeneralSecurityException, IOException {
        return new TLSKeyManager(toKeyStore(type, path, password), password);
    }

    public final TLSTrustManager getTrustManager(final String type, final String path, final char[] password)
            throws GeneralSecurityException, IOException {
        return new TLSTrustManager(toKeyStore(type, path, password));
    }

    public final KeyStore toKeyStore(final String type, final String path, final char[] password)
            throws GeneralSecurityException, IOException {
        final KeyStore keyStore = KeyStore.getInstance(type);
        final byte[] bytesKeyStore = StreamU.read(new File(path));
        keyStore.load(new ByteArrayInputStream(bytesKeyStore), password);
        return keyStore;
    }
}
