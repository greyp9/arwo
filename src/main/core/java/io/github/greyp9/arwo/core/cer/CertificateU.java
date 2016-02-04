package io.github.greyp9.arwo.core.cer;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.lang.StringU;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.cert.CertificateEncodingException;
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

    public static byte[] toBytes(final X509Certificate certificate) throws CertificateEncodingException, IOException {
        final ByteArrayOutputStream os = new ByteArrayOutputStream();
        os.write(UTF8Codec.toBytes(Const.BEGIN_CERTIFICATE + Http.Token.LF));
        os.write(UTF8Codec.toBytes(StringU.splitToWidth(
                Base64Codec.encode(certificate.getEncoded()), Const.WIDTH_PEM_LINE, Http.Token.LF)));
        os.write(UTF8Codec.toBytes(Const.END_CERTIFICATE));
        return os.toByteArray();
    }

    private static class Const {
        private static final String X509 = "X.509";

        private static final String BEGIN_CERTIFICATE = "-----BEGIN CERTIFICATE-----";  // i18n internal
        private static final String END_CERTIFICATE = "-----END CERTIFICATE-----";  // i18n internal

        private static final int WIDTH_PEM_LINE = 64;
    }
}
