package io.github.greyp9.arwo.app.webdav.connection;

import com.github.sardine.Sardine;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.CursorWebDAV;
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
import io.github.greyp9.arwo.lib.sardine.webdav.connection.SardineFactory;
import io.github.greyp9.arwo.lib.sardine.webdav.connection.WebDAVConnection;

import java.io.IOException;
import java.net.URL;

public class WebDAVConnectionFactory implements ConnectionFactory {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final Alerts alerts;

    public WebDAVConnectionFactory(final ServletHttpRequest httpRequest, final AppUserState userState,
                                   final Bundle bundle, final Alerts alerts) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.bundle = bundle;
        this.alerts = alerts;
    }

    @Override
    public final ConnectionResource create(final String name) throws IOException {
        ConnectionResource resource = null;
        final XedSession session = userState.getDocumentState().getSession(App.Servlet.SETTINGS);
        final CursorWebDAV cursor = new CursorWebDAV(session.getXed(), name);
        if (Value.isEmpty(name)) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.getString("SSHConnectionFactory.no.server")));
        } else if (cursor.getCursor() == null) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format("SSHConnectionFactory.no.config", name)));
        } else {
            resource = getConnection(name);
        }
        return resource;
    }

    private WebDAVConnectionResource getConnection(final String name) throws IOException {
        final XedSession session = userState.getDocumentState().getSession(App.Servlet.SETTINGS);
        final CursorWebDAV cursorX = new CursorWebDAV(session.getXed(), name);
        final String protocol = cursorX.getProtocol();
        final String host = cursorX.getHost();
        final int port = NumberU.toInt(cursorX.getPort(), 0);
        final String user = cursorX.getUser();
        final String password = cursorX.getPassword();
        final char[] secret = Value.toCharArray(httpRequest.getHeader(Http.Header.AUTHORIZATION));
        final XedCursor cursor = cursorX.getCursor();
        final TypeInstance instancePassword = cursor.getChildInstance("password");
        final KeyX keyPassword = XedKey.getKeyPBE(secret, instancePassword);
        final String passwordClear = ((password == null) ? null : keyPassword.unprotect(password));
        final String certificate = cursorX.getCertificate();
        final Sardine connection = new SardineFactory().createConnection(certificate);
        connection.setCredentials(user, passwordClear);
        final URL url = new URL(protocol, host, port, "");
        return new WebDAVConnectionResource(name, new WebDAVConnection(connection, url));
    }
}
