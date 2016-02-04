package io.github.greyp9.arwo.lib.mail.smtp.connection;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.config.CursorSMTP;
import io.github.greyp9.arwo.core.tls.context.TLSContext;
import io.github.greyp9.arwo.core.tls.context.TLSContextFactory;
import io.github.greyp9.arwo.core.tls.socket.TLSSocketFactory;
import io.github.greyp9.arwo.core.value.Value;

import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Properties;

public class SessionFactory {
    private final CursorSMTP cursorSMTP;
    private final Properties properties;

    public SessionFactory(final CursorSMTP cursorSMTP, final Properties properties) {
        this.cursorSMTP = cursorSMTP;
        this.properties = properties;
    }

    public final Session getSession() throws IOException {
        final Properties propertiesSession = new Properties();
        //properties.setProperty("mail.debug", "true");
        if ("starttls".equals(cursorSMTP.getProtocol())) {  // i18n revisit
            propertiesSession.setProperty("mail.smtp.starttls.enable", Boolean.TRUE.toString());
        } else if ("smtps".equals(cursorSMTP.getProtocol())) {  // i18n revisit
            propertiesSession.setProperty("mail.smtp.ssl.enable", Boolean.TRUE.toString());
        }
        setCustomSocketFactory(cursorSMTP.getCertificate(), propertiesSession);
        return Session.getInstance(propertiesSession);
    }

    public final Transport getTransport(final Session session) throws MessagingException {
        final Transport transport = session.getTransport("smtp");  // i18n lib
        transport.connect(cursorSMTP.getHost(), cursorSMTP.getPort(),
                cursorSMTP.getUser(), properties.getProperty(App.Settings.PASSWORD));  // i18n
        return transport;
    }

    private static void setCustomSocketFactory(
            final String certificate, final Properties properties) throws IOException {
        if (!Value.isEmpty(certificate)) {
            try {
                final TLSContext tlsContext = new TLSContextFactory().create(certificate, "TLS");  // i18n JRE
                TLSSocketFactory.initialize(tlsContext.getContext());
                properties.setProperty("mail.smtp.ssl.socketFactory.class", TLSSocketFactory.class.getName());
                properties.setProperty("mail.smtp.ssl.socketFactory.fallback", Boolean.FALSE.toString());
            } catch (GeneralSecurityException e) {
                throw new IOException(e);
            }
        }
    }
}
