package io.github.greyp9.arwo.core.tls.context;

import io.github.greyp9.arwo.core.cer.CertificateU;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.tls.manage.TLSTrustManager;

import java.io.IOException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

public class TLSContextFactory {

    public TLSContext create(String pemCertificate, String protocol) throws IOException {
        return create(UTF8Codec.toBytes(pemCertificate), protocol);
    }

    public TLSContext create(byte[] bytesCertificate, String protocol) throws IOException {
        try {
            return create(CertificateU.toX509(bytesCertificate), protocol);
        } catch (CertificateException e) {
            throw new IOException(e);
        }
    }

    public TLSContext create(X509Certificate certificate, String protocol) {
        TLSTrustManager trustManager = new TLSTrustManager(new X509Certificate[] { certificate });
        return new TLSContext(null, trustManager, protocol);
    }
}
