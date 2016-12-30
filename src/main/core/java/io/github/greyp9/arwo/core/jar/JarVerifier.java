package io.github.greyp9.arwo.core.jar;

import io.github.greyp9.arwo.core.io.StreamU;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.logging.Logger;

public final class JarVerifier {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final Certificate certificate;

    public JarVerifier(Class<?> c) {
        this(c.getProtectionDomain().getCodeSource().getCertificates());
    }

    private JarVerifier(Certificate[] certificates) {
        this.certificate = ((certificates == null) ? null : certificates[0]);
    }

    public void verify(JarFile jarFile) throws IOException, GeneralSecurityException {
        final Enumeration<? extends JarEntry> entries = jarFile.entries();
        while (entries.hasMoreElements()) {
            verify(jarFile, entries.nextElement());
        }
        logger.info(jarFile.getName());
    }

    private void verify(JarFile jarFile, JarEntry entry) throws IOException, GeneralSecurityException {
        // check the integrity of the entry
        StreamU.read(jarFile.getInputStream(entry));
        // check the entry signer against the baseline code signer
        if (!entry.isDirectory()) {
            final String name = entry.getName();
            if (!name.startsWith("META-INF/")) {
                verify(entry, name);
            }
        }
    }

    private void verify(final JarEntry entry, final String name) throws GeneralSecurityException {
        final Certificate[] certificates = entry.getCertificates();
        if (certificate == null) {
            logger.finest(name);
        } else if (certificates == null) {
            logger.severe(name);
            throw new CertificateException(entry.getName());
        } else {
            logger.finest(String.format("%s, [%d]", name, certificates.length));
            final Certificate certificateEntry = certificates[0];
            certificateEntry.verify(certificate.getPublicKey());
        }
    }
}
