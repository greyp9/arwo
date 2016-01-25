package io.github.greyp9.arwo.lib.ganymed.ssh.server;

import ch.ethz.ssh2.ConnectionInfo;
import ch.ethz.ssh2.KnownHosts;
import ch.ethz.ssh2.signature.RSASHA1Verify;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.hash.text.FingerPrint;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.KeyFactory;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.RSAPublicKeySpec;

public class TrustVerifier {
    private final String hostName;
    private final KnownHosts knownHosts;

    public TrustVerifier(final String hostName, final String algorithm, final byte[] key) throws IOException {
        this.hostName = hostName;
        this.knownHosts = new KnownHosts();
        if (Value.notEmpty(hostName, algorithm, UTF8Codec.toString(key))) {
            final String[] hostNames = { hostName };
            this.knownHosts.addHostkey(hostNames, algorithm, key);
        }
    }

    public final int verify(final ConnectionInfo connectionInfo) throws IOException {
        final String algorithm = connectionInfo.serverHostKeyAlgorithm;
        final byte[] key = connectionInfo.serverHostKey;
        return knownHosts.verifyHostkey(hostName, algorithm, key);
    }

    public static class Alerter {
        private final Bundle bundle;
        private final Alerts alerts;

        public Alerter(final Bundle bundle, final Alerts alerts) {
            this.bundle = bundle;
            this.alerts = alerts;
        }

        public final void addAlertsStored(final ServerParams params, final Alert.Severity severity) throws IOException {
            final String algorithm = params.getAlgorithm();
            final byte[] publicKey = params.getPublicKey();
            if (Value.isEmpty(algorithm) || Value.isEmpty(publicKey)) {
                alerts.add(new Alert(severity, bundle.getString("TrustVerifier.stored.missing-info")));
            } else if (Const.LIB_ALGORITHM_RSA.equals(algorithm)) {
                alerts.add(new Alert(severity, bundle.format("TrustVerifier.stored.info", params.getHost())));
                addAlertsRSA(publicKey, severity);
            } else {
                alerts.add(new Alert(severity, bundle.getString("TrustVerifier.unsupported-algorithm")));
            }
        }

        public final void addAlertsConnection(
                final ServerParams params, final String algorithm,
                final byte[] publicKey, final Alert.Severity severity) throws IOException {
            if (Const.LIB_ALGORITHM_RSA.equals(algorithm)) {
                alerts.add(new Alert(severity, bundle.format("TrustVerifier.connection.info", params.getHost())));
                addAlertsRSA(publicKey, severity);
            } else {
                alerts.add(new Alert(severity, bundle.getString("TrustVerifier.unsupported-algorithm")));
            }
        }

        private void addAlertsRSA(final byte[] publicKeyBytes, final Alert.Severity severity) throws IOException {
            final RSAPublicKey publicKey = getPublicKey(publicKeyBytes);
            alerts.add(new Alert(severity, bundle.format("TrustVerifier.key.info",
                    publicKey.getAlgorithm(), publicKey.getModulus().bitLength())));
            alerts.add(new Alert(severity, bundle.format("TrustVerifier.key.MD5",
                    FingerPrint.toHex(HashU.md5(publicKeyBytes)))));
            alerts.add(new Alert(severity, bundle.format("TrustVerifier.key.SHA1",
                    FingerPrint.toHex(HashU.sha1(publicKeyBytes)))));
            alerts.add(new Alert(severity, Base64Codec.encode(publicKeyBytes)));
        }

        private RSAPublicKey getPublicKey(final byte[] publicKeyBytes) throws IOException {
            RSAPublicKey publicKey;
            try {
                final ch.ethz.ssh2.signature.RSAPublicKey publicKeyLib =
                        RSASHA1Verify.decodeSSHRSAPublicKey(publicKeyBytes);
                final KeyFactory keyFactory = KeyFactory.getInstance(Const.ALGORITHM_RSA);
                final RSAPublicKeySpec keySpec = new RSAPublicKeySpec(publicKeyLib.getN(), publicKeyLib.getE());
                publicKey = (RSAPublicKey) keyFactory.generatePublic(keySpec);
            } catch (GeneralSecurityException e) {
                alerts.add(new Alert(Alert.Severity.ERR, e.getMessage()));
                throw new IOException(e);
            }
            return publicKey;
        }

        private static class Const {
            private static final String ALGORITHM_RSA = "RSA";  // i18n lib
            private static final String LIB_ALGORITHM_RSA = "ssh-rsa";  // i18n lib
        }
    }
}
