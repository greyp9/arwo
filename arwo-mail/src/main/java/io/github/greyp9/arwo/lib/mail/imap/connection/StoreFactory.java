package io.github.greyp9.arwo.lib.mail.imap.connection;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.tls.alert.AlertCertificate;
import io.github.greyp9.arwo.core.tls.context.TLSContext;
import io.github.greyp9.arwo.core.tls.context.TLSContextFactory;
import io.github.greyp9.arwo.core.tls.socket.TLSSocketFactory;
import io.github.greyp9.arwo.core.value.Value;

import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Store;
import javax.net.ssl.SSLException;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Properties;

public class StoreFactory {
    private final String protocol;
    private final String host;
    private final int port;
    private final String user;
    private final String password;
    private final String certificate;

    public StoreFactory(final String protocol, final String host, final int port,
                        final String user, final String password, final String certificate) {
        this.protocol = protocol;
        this.host = host;
        this.port = port;
        this.user = user;
        this.password = password;
        this.certificate = certificate;
    }

    public final Store getStore(
            final Bundle bundle, final Alerts alerts, final ResourceCache cacheBlob) throws IOException {
        final Properties properties = new Properties();
        //properties.setProperty("mail.debug", "true");
        properties.setProperty("mail.store.protocol", protocol);
        setCustomSocketFactory(certificate, properties);
        final Session session = Session.getInstance(properties);
        try {
            final Store store = session.getStore(protocol);
            store.connect(host, port, user, password);
            return store;
        } catch (MessagingException e) {
            final boolean enabled = (e.getCause() instanceof SSLException);
            new AlertCertificate(bundle, alerts, cacheBlob).alert(enabled, host, port, Const.PROTOCOL);
            throw new IOException(e);
        }
    }

    private static void setCustomSocketFactory(
            final String certificate, final Properties properties) throws IOException {
        if (!Value.isEmpty(certificate)) {
            try {
                final TLSContext tlsContext = new TLSContextFactory().create(certificate, Const.PROTOCOL);
                TLSSocketFactory.initialize(tlsContext.getContext());
                properties.setProperty("mail.imaps.socketFactory.class", TLSSocketFactory.class.getName());
                properties.setProperty("mail.imaps.socketFactory.fallback", Boolean.FALSE.toString());
            } catch (GeneralSecurityException e) {
                throw new IOException(e);
            }
        }
    }

    private static class Const {
        private static final String PROTOCOL = "TLS";  // i18n JRE
    }
}
