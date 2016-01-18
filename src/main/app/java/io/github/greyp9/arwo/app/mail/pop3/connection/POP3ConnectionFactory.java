package io.github.greyp9.arwo.app.mail.pop3.connection;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.CursorPOP3;
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
import io.github.greyp9.arwo.lib.mail.pop3.connection.POP3Connection;
import io.github.greyp9.arwo.lib.mail.pop3.connection.StoreFactory;

import javax.mail.Store;
import java.io.IOException;

public class POP3ConnectionFactory implements ConnectionFactory {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final Alerts alerts;

    public POP3ConnectionFactory(final ServletHttpRequest httpRequest, final AppUserState userState,
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
        final CursorPOP3 cursorPOP3 = new CursorPOP3(session.getXed(), name);
        if (Value.isEmpty(name)) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.getString("SSHConnectionFactory.no.server")));
        } else if (cursorPOP3.getCursor() == null) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format("SSHConnectionFactory.no.config", name)));
        } else {
            resource = getConnection(name, cursorPOP3);
        }
        return resource;
    }

    private POP3ConnectionResource getConnection(final String name, final CursorPOP3 cursorPOP3) throws IOException {
        final String protocol = cursorPOP3.getProtocol();
        final String host = cursorPOP3.getHost();
        final Integer port = NumberU.toInt(cursorPOP3.getPort(), 995);
        final String user = cursorPOP3.getUser();
        final String password = cursorPOP3.getPassword();
        final char[] secret = Value.toCharArray(httpRequest.getHeader(Http.Header.AUTHORIZATION));
        final XedCursor cursor = cursorPOP3.getCursor();
        final TypeInstance instancePassword = cursor.getChildInstance("password");
        final KeyX keyPassword = XedKey.getKeyPBE(secret, instancePassword);
        final String passwordClear = ((password == null) ? null : keyPassword.unprotect(password));
        final String certificate = cursorPOP3.getCertificate();
        final Store store = new StoreFactory(protocol, host, port, user, passwordClear, certificate).getStore();
        final POP3Connection connection = new POP3Connection(store);
        return new POP3ConnectionResource(name, connection);
    }
}
