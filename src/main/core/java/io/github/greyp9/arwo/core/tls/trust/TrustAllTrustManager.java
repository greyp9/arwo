package io.github.greyp9.arwo.core.tls.trust;

import javax.net.ssl.X509TrustManager;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

public class TrustAllTrustManager implements X509TrustManager {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public final void checkClientTrusted(
            final X509Certificate[] x509Certificates, final String s) throws CertificateException {
        logger.finest("checkClientTrusted()");
    }

    @Override
    public final void checkServerTrusted(
            final X509Certificate[] x509Certificates, final String s) throws CertificateException {
        logger.finest("checkServerTrusted()");
    }

    @Override
    public final X509Certificate[] getAcceptedIssuers() {
        return new X509Certificate[0];
    }
}
