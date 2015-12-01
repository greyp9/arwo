package io.github.greyp9.arwo.app.ssh.connection;

import ch.ethz.ssh2.Connection;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.lib.ganymed.ssh.client.Authenticator;
import io.github.greyp9.arwo.lib.ganymed.ssh.client.ClientParams;

import java.io.IOException;

public class SSHAuthenticatorClient {
    private final Bundle bundle;
    private final Alerts alerts;

    public SSHAuthenticatorClient(final Bundle bundle, final Alerts alerts) {
        this.bundle = bundle;
        this.alerts = alerts;
    }

    public final void authenticate(final Connection connection, final ClientParams clientParams) throws IOException {
        final Authenticator authenticator = new Authenticator(clientParams, bundle, alerts);
        authenticator.authenticate(clientParams.getName(), connection);
    }
}
