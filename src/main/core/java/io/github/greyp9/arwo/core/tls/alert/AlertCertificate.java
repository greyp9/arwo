package io.github.greyp9.arwo.core.tls.alert;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.write.AlertWriter;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.cer.CertificateU;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.tls.client.CertificateClient;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Collection;

public class AlertCertificate {
    private final Bundle bundle;
    private final Alerts alerts;
    private final ResourceCache cacheBlob;

    public AlertCertificate(final Bundle bundle, final Alerts alerts, final ResourceCache cacheBlob) {
        this.bundle = bundle;
        this.alerts = alerts;
        this.cacheBlob = cacheBlob;
    }

    public final void alert(final boolean enabled, final String host, final int port, final String protocol) {
        if (enabled) {
            final String message = bundle.getString("AlertCertificate.error.message");
            try {
                final URL url = new URL(String.format("https://%s:%s", host, port));
                final CertificateClient client = new CertificateClient(protocol);
                alert(client.getCertificateChain(url));
            } catch (GeneralSecurityException e) {
                alerts.add(new Alert(Alert.Severity.ERR, message, e.getMessage()));
            } catch (IOException e) {
                alerts.add(new Alert(Alert.Severity.ERR, message, e.getMessage()));
            }
        }
    }

    private void alert(final Collection<X509Certificate> certificates)
            throws IOException, CertificateEncodingException {
        for (final X509Certificate certificateIt : certificates) {
            alert(certificateIt);
        }
    }

    private void alert(final X509Certificate certificate)
            throws CertificateEncodingException, IOException {
        final byte[] bytes = CertificateU.toBytes(certificate);
        // cache blob
        final String resource = Http.Token.SLASH + Long.toHexString(CRCU.crc32(bytes)) + ".cer";
        cacheBlob.putFile(resource, new MetaFile(null, Http.Mime.APP_CERT, new ByteArrayInputStream(bytes)));
        final String href = cacheBlob.getEndpoint() + resource;
        new AlertWriter(bundle, alerts).write("AlertCertificate.message", "AlertCertificate.download", href);
        //final String detail = UTF8Codec.toString(bytes);
        //alerts.add(new Alert(Alert.Severity.INFO, certificateIt.getType(), detail));
    }
}
