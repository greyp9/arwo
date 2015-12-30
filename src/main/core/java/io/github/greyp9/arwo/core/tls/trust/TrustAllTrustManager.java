package io.github.greyp9.arwo.core.tls.trust;

import javax.net.ssl.X509TrustManager;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

public class TrustAllTrustManager implements X509TrustManager {

    @Override
    public final void checkClientTrusted(
            final X509Certificate[] x509Certificates, final String s) throws CertificateException {
        s.getClass();
    }

    @Override
    public final void checkServerTrusted(
            final X509Certificate[] x509Certificates, final String s) throws CertificateException {
        s.getClass();
    }

    @Override
    public final X509Certificate[] getAcceptedIssuers() {
        return new X509Certificate[0];
    }
}
