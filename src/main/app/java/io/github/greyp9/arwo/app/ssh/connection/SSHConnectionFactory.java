package io.github.greyp9.arwo.app.ssh.connection;

import ch.ethz.ssh2.Connection;
import ch.ethz.ssh2.ConnectionInfo;
import ch.ethz.ssh2.HTTPProxyData;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.config.CursorSSH;
import io.github.greyp9.arwo.core.connect.ConnectionFactory;
import io.github.greyp9.arwo.core.connect.ConnectionResource;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.jce.KeyX;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.extension.XedKey;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.lib.ganymed.ssh.client.ClientParams;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;
import io.github.greyp9.arwo.lib.ganymed.ssh.server.ServerParams;

import java.io.IOException;
import java.net.URI;
import java.util.logging.Logger;

public class SSHConnectionFactory implements ConnectionFactory {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final Alerts alerts;
    private final ResourceCache cacheBlob;

    public SSHConnectionFactory(final ServletHttpRequest httpRequest, final AppUserState userState,
                                final Bundle bundle, final Alerts alerts) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.bundle = bundle;
        this.alerts = alerts;
        this.cacheBlob = userState.getCacheBlob();
    }

    @Override
    public final ConnectionResource create(final String name) throws IOException {
        ConnectionResource resource = null;
        final XedSession session = userState.getDocumentState().getSession(App.Servlet.SETTINGS);
        final CursorSSH cursorSSH = new CursorSSH(session.getXed(), name);
        if (Value.isEmpty(name)) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.getString("SSHConnectionFactory.no.server")));
        } else if (cursorSSH.getCursor() == null) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format("SSHConnectionFactory.no.config", name)));
        } else {
            resource = getConnection(name, cursorSSH);
        }
        return resource;
    }

    @SuppressWarnings("PMD.CloseResource")
    private SSHConnectionResource getConnection(final String name, final CursorSSH cursorSSH) throws IOException {
        final String host = cursorSSH.getHost();
        final Integer port = NumberU.toInt(cursorSSH.getPort(), 22);
        final String proxy = cursorSSH.getProxy();
        Logger.getLogger(getClass().getName()).info(proxy);
        final URI uriProxy = Value.isEmpty(proxy) ? null : URLCodec.toURI(proxy);
        final String term = Value.defaultOnEmpty(cursorSSH.getTerm(), null);
        final String user = cursorSSH.getUser();
        final String password = cursorSSH.getPassword();
        final String privateKey = cursorSSH.getPrivateKey();
        final char[] secret = Value.toCharArray(httpRequest.getHeader(Http.Header.AUTHORIZATION));
        final XedCursor cursor = cursorSSH.getCursor();
        final TypeInstance instancePassword = cursor.getChildInstance(
                App.Settings.AUTH_PASSWORD).getInstance(App.Settings.PASSWORD);
        final KeyX keyPassword = XedKey.getKeyPBE(secret, instancePassword);
        final String passwordClear = ((password == null) ? null : keyPassword.unprotect(password));
        final String privateKeyClear = ((privateKey == null) ? null : keyPassword.unprotect(privateKey));
        final String algorithm = cursorSSH.getAlgorithm();
        final byte[] publicKey = Base64Codec.decode(cursorSSH.getPublicKey());
        final ServerParams serverParams = new ServerParams(name, host, algorithm, publicKey);
        final ClientParams clientParams = new ClientParams(name, user, passwordClear, privateKeyClear);
        final HTTPProxyData proxyData = ((uriProxy == null) ? null :
                new HTTPProxyData(uriProxy.getHost(), uriProxy.getPort()));
        final Connection connection = new Connection(host, port, proxyData);
        final ConnectionInfo connectionInfo = connection.connect();
        new SSHAuthenticatorServer(bundle, alerts, cacheBlob).authenticate(connectionInfo, serverParams);
        new SSHAuthenticatorClient(bundle, alerts).authenticate(connection, clientParams);
        return new SSHConnectionResource(name, new SSHConnection(connection, term));
    }
}
