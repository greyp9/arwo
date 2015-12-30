package io.github.greyp9.arwo.lib.sardine.webdav.connection;

import com.github.sardine.Sardine;
import com.github.sardine.impl.SardineImpl;
import io.github.greyp9.arwo.core.tls.context.TLSContext;
import io.github.greyp9.arwo.core.tls.context.TLSContextFactory;
import io.github.greyp9.arwo.core.tls.verifier.TrustAllHostnameVerifier;
import io.github.greyp9.arwo.core.value.Value;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;

import javax.net.ssl.HostnameVerifier;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.logging.Level;
import java.util.logging.Logger;

public class SardineFactory {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public final Sardine createConnection(final String certificate) throws IOException {
        return (Value.isData(certificate) ? createConnectionNN(certificate) : new SardineImpl());
    }

    private Sardine createConnectionNN(final String certificate) throws IOException {
        final TLSContext tlsContext = new TLSContextFactory().create(certificate, "TLS");
        final HostnameVerifier verifier = new TrustAllHostnameVerifier();
        return new SardineImpl() {
            @Override
            protected ConnectionSocketFactory createDefaultSecureSocketFactory() {
                ConnectionSocketFactory factory = null;
                try {
                    factory = new SSLConnectionSocketFactory(tlsContext.getContext(), verifier);
                } catch (GeneralSecurityException e) {
                    logger.log(Level.SEVERE, e.getMessage(), e);
                }
                return factory;
            }
        };
    }
}
