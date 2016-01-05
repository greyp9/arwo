package io.github.greyp9.arwo.lib.ganymed.ssh.client;

import ch.ethz.ssh2.Connection;
import ch.ethz.ssh2.InteractiveCallback;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.AlertU;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Properties;

public class Authenticator {
    private final ClientParams clientParams;
    private final Bundle bundle;
    private final Alerts alerts;

    public Authenticator(final ClientParams clientParams, final Bundle bundle, final Alerts alerts) {
        this.clientParams = clientParams;
        this.bundle = bundle;
        this.alerts = alerts;
    }

    public final void authenticate(final String serverName, final Connection connection) throws IOException {
        alerts.add(new Alert(Alert.Severity.INFO, bundle.format("Authenticator.attempt", serverName)));
        final String user = clientParams.getUser();
        final boolean authenticated = (authenticatePublicKey(user, connection) ||
                authenticateInteractive(user, connection) ||
                authenticatePassword(user, connection));
        final boolean authComplete = connection.isAuthenticationComplete();
        final String messageKey = (authComplete ? "Authenticator.success" : "Authenticator.failure");
        alerts.add(new Alert(AlertU.toInfoWarn(authenticated), bundle.format(messageKey, serverName)));
        if (!authenticated) {
            //String message = "ssh-copy-id -i ~/.ssh/id_rsa.pub user@host";
            //alerts.add(new Alert(Alert.Severity.INFO, getClass().getName(), message));
            throw new IOException(new SecurityException(user));
        }
    }

    private boolean authenticatePublicKey(final String user, final Connection connection) throws IOException {
        boolean authenticated = connection.isAuthenticationComplete();
        final boolean methodPublicKey = connection.isAuthMethodAvailable(user, Const.PUBLIC_KEY);
        final boolean isPrivateKey = (!Value.isEmpty(clientParams.getPrivateKey()));
        if ((!authenticated) && (methodPublicKey) && (isPrivateKey)) {
            final char[] privateKey = clientParams.getPrivateKey().toCharArray();
            authenticated = connection.authenticateWithPublicKey(user, privateKey, null);
            final String message = bundle.getString(authenticated ?
                    "Authenticator.publicKey.success" : "Authenticator.publicKey.failure");
            alerts.add(new Alert(AlertU.toInfoWarn(authenticated), message));
        }
        return authenticated;
    }

    private boolean authenticateInteractive(final String user, final Connection connection) throws IOException {
        boolean authenticated = connection.isAuthenticationComplete();
        final boolean methodInteractive = connection.isAuthMethodAvailable(user, Const.INTERACTIVE);
        final boolean isPassword = (!Value.isEmpty(clientParams.getPassword()));
        if ((!authenticated) && (methodInteractive) && (isPassword)) {
            final Properties properties = new Properties();
            properties.setProperty("//Password: ", new String(clientParams.getPassword().toCharArray()));  // i18n
            final InteractiveCallback callback = new InteractiveCallbackImpl(properties);
            authenticated = connection.authenticateWithKeyboardInteractive(user, callback);
            final String message = bundle.getString(authenticated ?
                    "Authenticator.keyboardInteractive.success" : "Authenticator.keyboardInteractive.failure");
            alerts.add(new Alert(AlertU.toInfoWarn(authenticated), message));
        }
        return authenticated;
    }

    private boolean authenticatePassword(final String user, final Connection connection) throws IOException {
        boolean authenticated = connection.isAuthenticationComplete();
        final boolean methodPassword = connection.isAuthMethodAvailable(user, Const.PASSWORD);
        final boolean isPassword = (!Value.isEmpty(clientParams.getPassword()));
        if ((!authenticated) && (methodPassword) && (isPassword)) {
            authenticated = connection.authenticateWithPassword(user, clientParams.getPassword());
            final String message = bundle.getString(authenticated ?
                    "Authenticator.password.success" : "Authenticator.password.failure");
            alerts.add(new Alert(AlertU.toInfoWarn(authenticated), message));
        }
        return authenticated;
    }

    public static class InteractiveCallbackImpl implements InteractiveCallback {
        private final Properties properties;

        public InteractiveCallbackImpl(final Properties properties) {
            this.properties = properties;
        }

        @SuppressWarnings("PMD.UseVarargs")
        public final String[] replyToChallenge(
                final String name, final String instruction, final int numPrompts,
                final String[] prompt, final boolean[] echo) throws GeneralSecurityException {
            String[] replies = new String[numPrompts];
            for (int i = 0; (i < numPrompts); i++) {
                final String key = String.format("%s/%s/%s", name, instruction, prompt[i]);
                //alerts.add(new Alert(Alert.Severity.INFO, key));
                final String value = properties.getProperty(key);
                if (value == null) {
                    throw new GeneralSecurityException(key);
                } else {
                    replies[i] = value;
                }
            }
            return replies;
        }
    }

    private static class Const {
        private static final String PUBLIC_KEY = "publickey";  // i18n
        private static final String INTERACTIVE = "keyboard-interactive";  // i18n
        private static final String PASSWORD = "password";  // i18n
    }
}
