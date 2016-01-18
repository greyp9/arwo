package io.github.greyp9.arwo.app.jdbc.connection;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.model.ExceptionModel;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.CursorJDBC;
import io.github.greyp9.arwo.core.connect.ConnectionFactory;
import io.github.greyp9.arwo.core.connect.ConnectionResource;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.jce.KeyX;
import io.github.greyp9.arwo.core.jdbc.connection.JDBCConnection;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.extension.XedKey;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

import java.io.IOException;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.SQLException;
import java.util.Properties;

public class JDBCConnectionFactory implements ConnectionFactory {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final Alerts alerts;

    public JDBCConnectionFactory(final ServletHttpRequest httpRequest, final AppUserState userState,
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
        final CursorJDBC cursorJDBC = new CursorJDBC(session.getXed(), name);
        if (Value.isEmpty(name)) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.getString("SSHConnectionFactory.no.server")));
        } else if (cursorJDBC.getCursor() == null) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format("SSHConnectionFactory.no.config", name)));
        } else {
            resource = getConnection(name, cursorJDBC);
        }
        return resource;
    }

    @SuppressWarnings("PMD.CloseResource")
    private ConnectionResource getConnection(final String name, final CursorJDBC cursorJDBC) throws IOException {
        final String driverClass = cursorJDBC.getDriverClass();
        final String url = cursorJDBC.getURL();
        final String user = cursorJDBC.getUser();
        final String password = cursorJDBC.getPassword();
        final char[] secret = Value.toCharArray(httpRequest.getHeader(Http.Header.AUTHORIZATION));
        final XedCursor cursor = cursorJDBC.getCursor();
        final TypeInstance instancePassword = cursor.getChildInstance("password");
        final KeyX keyPassword = XedKey.getKeyPBE(secret, instancePassword);
        final String passwordClear = ((password == null) ? null : keyPassword.unprotect(password));
        final Properties properties = new Properties();
        PropertiesU.setProperty(properties, "user", user);
        PropertiesU.setProperty(properties, "password", passwordClear);
        final Connection connection = getConnection(driverClass, url, properties);
        return ((connection == null) ? null : new JDBCConnectionResource(name, new JDBCConnection(connection)));
    }

    private Connection getConnection(
            final String driverClassName, final String url, final Properties properties) throws IOException {
        Connection connection = null;
        try {
            final Class<?> driverClass = Class.forName(driverClassName);
            final Driver driver = (Driver) driverClass.newInstance();
            connection = driver.connect(url, properties);
        } catch (IllegalAccessException e) {
            new ExceptionModel(alerts).service(new IOException(e), Alert.Severity.ERR);
        } catch (InstantiationException e) {
            new ExceptionModel(alerts).service(new IOException(e), Alert.Severity.ERR);
        } catch (SQLException e) {
            new ExceptionModel(alerts).service(new IOException(e), Alert.Severity.ERR);
        } catch (ClassNotFoundException e) {
            new ExceptionModel(alerts).service(new IOException(e), Alert.Severity.ERR);
        }
        return connection;
    }
}
