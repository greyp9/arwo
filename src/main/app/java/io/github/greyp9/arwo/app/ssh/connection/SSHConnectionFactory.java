package io.github.greyp9.arwo.app.ssh.connection;

import ch.ethz.ssh2.Connection;
import ch.ethz.ssh2.ConnectionInfo;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.AppText;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.config.CursorSSH;
import io.github.greyp9.arwo.core.connect.ConnectionFactory;
import io.github.greyp9.arwo.core.connect.ConnectionResource;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.jce.KeyX;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.extension.XedKey;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.lib.ganymed.ssh.client.ClientParams;
import io.github.greyp9.arwo.lib.ganymed.ssh.server.ServerParams;

import java.io.IOException;

public class SSHConnectionFactory implements ConnectionFactory {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public SSHConnectionFactory(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    @Override
    public final ConnectionResource create(final String name) throws IOException {
        return getConnection(name);
    }

    @SuppressWarnings("PMD.CloseResource")
    private SSHConnectionResource getConnection(final String name) throws IOException {
        final XedSession session = userState.getDocumentState().getSession("/app");
        final CursorSSH cursorSSH = new CursorSSH(session.getXed(), name);
        final String host = cursorSSH.getHost();
        final Integer port = NumberU.toInt(cursorSSH.getPort(), 22);
        final String user = cursorSSH.getUser();
        final String password = cursorSSH.getPassword();
        final String privateKey = cursorSSH.getPrivateKey();
        final char[] secret = Value.toCharArray(httpRequest.getHeader(Http.Header.AUTHORIZATION));
        final XedCursor cursor = cursorSSH.getCursor();
        final TypeInstance instancePassword = cursor.getChildInstance("authPassword").getInstance("password");
        final KeyX keyPassword = XedKey.getKeyPBE(secret, instancePassword);
        final String passwordClear = ((password == null) ? null : keyPassword.unprotect(password));
        final String privateKeyClear = ((privateKey == null) ? null : keyPassword.unprotect(privateKey));
        final String algorithm = cursorSSH.getAlgorithm();
        final byte[] publicKey = Base64Codec.decode(cursorSSH.getPublicKey());
        final ServerParams serverParams = new ServerParams(name, host, algorithm, publicKey);
        final ClientParams clientParams = new ClientParams(name, user, passwordClear, privateKeyClear);
        final Connection connection = new Connection(host, port);
        final ConnectionInfo connectionInfo = connection.connect();
        final Bundle bundle = new Bundle(new AppText(userState.getLocus().getLocale()).getBundleCore());
        final Alerts alerts = userState.getAlerts();
        new SSHAuthenticatorServer(bundle, alerts).authenticate(connectionInfo, serverParams);
        new SSHAuthenticatorClient(bundle, alerts).authenticate(connection, clientParams);
        return new SSHConnectionResource(name, new SSHConnection(connection));
    }
}
