package io.github.greyp9.arwo.app.ssh.connection;

import ch.ethz.ssh2.ConnectionInfo;
import ch.ethz.ssh2.KnownHosts;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.lib.ganymed.ssh.server.ServerParams;
import io.github.greyp9.arwo.lib.ganymed.ssh.server.TrustVerifier;

import java.io.IOException;
import java.security.KeyException;

public class SSHAuthenticatorServer {
    private final Bundle bundle;
    private final Alerts alerts;

    public SSHAuthenticatorServer(final Bundle bundle, final Alerts alerts) {
        this.bundle = bundle;
        this.alerts = alerts;
    }

    public final void authenticate(final ConnectionInfo connectionInfo, final ServerParams params) throws IOException {
        final TrustVerifier verifier = new TrustVerifier(
                params.getHost(), params.getAlgorithm(), params.getPublicKey());
        final boolean isTrusted = (verifier.verify(connectionInfo) == KnownHosts.HOSTKEY_IS_OK);
        if (!isTrusted) {
            final TrustVerifier.Alerter alerter = new TrustVerifier.Alerter(bundle, alerts);
            alerter.addAlertsStored(params, Alert.Severity.WARN);
            final String serverAlgorithm = connectionInfo.serverHostKeyAlgorithm;
            final byte[] serverPublicKey = connectionInfo.serverHostKey;
            alerter.addAlertsConnection(params, serverAlgorithm, serverPublicKey, Alert.Severity.WARN);
            throw new IOException(new KeyException(params.getHost()));
        }
    }
}
