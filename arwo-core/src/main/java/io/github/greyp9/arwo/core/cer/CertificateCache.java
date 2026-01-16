package io.github.greyp9.arwo.core.cer;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.tls.client.CertificateClient;
import io.github.greyp9.arwo.core.tls.verifier.TrustAllHostnameVerifier;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.Date;
import java.util.logging.Logger;

public final class CertificateCache {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final File folder;

    public CertificateCache(final File folder) {
        this.folder = folder;
    }

    public X509Certificate getCertificate(final URL url) throws IOException, GeneralSecurityException {
        final X509Certificate certificate;
        final String filename = String.format("cer.%s.cer", url.getHost());
        final File fileCertificate = new File(folder, filename);
        if (!url.getProtocol().equals("https")) {
            certificate = null;
        } else if (fileCertificate.exists()) {
            certificate = CertificateU.toX509(StreamU.read(fileCertificate));
            logger.fine(String.format("certificate for host %s (validity from %s to %s) read from %s", url.getHost(),
                    certificate.getNotBefore(), certificate.getNotAfter(), fileCertificate.getAbsolutePath()));
        } else {
            final CertificateClient client = new CertificateClient("TLSv1.2", new TrustAllHostnameVerifier());
            final Collection<X509Certificate> certificates = client.getCertificateChain(url);
            certificate = certificates.iterator().next();
            StreamU.write(fileCertificate, CertificateU.toBytes(certificate));
            logger.fine(String.format("certificate for host %s (validity from %s to %s) written to %s", url.getHost(),
                    certificate.getNotBefore(), certificate.getNotAfter(), fileCertificate.getAbsolutePath()));
        }
        if ((certificate != null)
                && (DurationU.add(new Date(), DateU.Const.TZ_GMT, "P28D").after(certificate.getNotAfter()))) {
            logger.info(String.format("EXPIRING SOON: certificate for host %s (validity from %s to %s)",
                    url.getHost(), certificate.getNotBefore(), certificate.getNotAfter()));
        }
        return certificate;
    }
}
