package io.github.greyp9.arwo.app.mail.smtp.connection;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.CursorSMTP;
import io.github.greyp9.arwo.core.connect.ConnectionFactory;
import io.github.greyp9.arwo.core.connect.ConnectionResource;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.jce.KeyX;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.extension.XedKey;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.lib.mail.smtp.connection.SMTPConnection;

import java.io.IOException;

public class SMTPConnectionFactory implements ConnectionFactory {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final Alerts alerts;

    public SMTPConnectionFactory(final ServletHttpRequest httpRequest, final AppUserState userState,
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
        final CursorSMTP cursorSMTP = new CursorSMTP(session.getXed(), name);
        if (Value.isEmpty(name)) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.getString("SSHConnectionFactory.no.server")));
        } else if (cursorSMTP.getCursor() == null) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format("SSHConnectionFactory.no.config", name)));
        } else {
            resource = getConnection(name, cursorSMTP);
        }
        return resource;
    }

    private SMTPConnectionResource getConnection(final String name, final CursorSMTP cursorSMTP) throws IOException {
        final String password = cursorSMTP.getPassword();
        final char[] secret = Value.toCharArray(httpRequest.getHeader(Http.Header.AUTHORIZATION));
        final XedCursor cursor = cursorSMTP.getCursor();
        final TypeInstance instancePassword = cursor.getChildInstance("password");
        final KeyX keyPassword = XedKey.getKeyPBE(secret, instancePassword);
        final String passwordClear = ((password == null) ? null : keyPassword.unprotect(password));
        final SMTPConnection connection = new SMTPConnection(cursorSMTP);
        PropertiesU.setProperty(connection.getProperties(), "password", passwordClear);
        return new SMTPConnectionResource(name, connection);
    }
}
