package io.github.greyp9.arwo.app.mail.imap.connection;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.CursorIMAP;
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
import io.github.greyp9.arwo.lib.mail.imap.connection.IMAPConnection;
import io.github.greyp9.arwo.lib.mail.imap.connection.StoreFactory;

import javax.mail.Store;
import java.io.IOException;

public class IMAPConnectionFactory implements ConnectionFactory {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final Alerts alerts;

    public IMAPConnectionFactory(final ServletHttpRequest httpRequest, final AppUserState userState,
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
        final CursorIMAP cursorIMAP = new CursorIMAP(session.getXed(), name);
        if (Value.isEmpty(name)) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.getString("SSHConnectionFactory.no.server")));
        } else if (cursorIMAP.getCursor() == null) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format("SSHConnectionFactory.no.config", name)));
        } else {
            resource = getConnection(name, cursorIMAP);
        }
        return resource;
    }

    private IMAPConnectionResource getConnection(final String name, final CursorIMAP cursorIMAP) throws IOException {
        final String protocol = cursorIMAP.getProtocol();
        final String host = cursorIMAP.getHost();
        final Integer port = NumberU.toInt(cursorIMAP.getPort(), 993);
        final String user = cursorIMAP.getUser();
        final String password = cursorIMAP.getPassword();
        final char[] secret = Value.toCharArray(httpRequest.getHeader(Http.Header.AUTHORIZATION));
        final XedCursor cursor = cursorIMAP.getCursor();
        final TypeInstance instancePassword = cursor.getChildInstance(App.Settings.PASSWORD);
        final KeyX keyPassword = XedKey.getKeyPBE(secret, instancePassword);
        final String passwordClear = ((password == null) ? null : keyPassword.unprotect(password));
        final String certificate = cursorIMAP.getCertificate();
        final Store store = new StoreFactory(protocol, host, port, user, passwordClear, certificate).getStore();
        final IMAPConnection connection = new IMAPConnection(store);
        return new IMAPConnectionResource(name, connection);
    }
}
