package io.github.greyp9.arwo.core.cer;

import java.io.ByteArrayInputStream;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

public final class CertificateU {

    private CertificateU() {
    }

    public static X509Certificate toX509(final byte[] bytes) throws CertificateException {
        final CertificateFactory factory = CertificateFactory.getInstance(Const.X509);
        return (X509Certificate) factory.generateCertificate(new ByteArrayInputStream(bytes));
    }

    private static class Const {
        private static final String X509 = "X.509";
    }
}
