package io.github.greyp9.arwo.core.tls.socket;

import javax.net.SocketFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.security.GeneralSecurityException;

@SuppressWarnings("PMD.AvoidSynchronizedAtMethodLevel")
public class TLSSocketFactory extends SSLSocketFactory {
    private static SSLSocketFactory wrappedFactory;

    public static synchronized void initialize(final SSLContext sslContext) throws GeneralSecurityException {
        wrappedFactory = sslContext.getSocketFactory();
    }

    public static synchronized SocketFactory getDefault() {
        return new TLSSocketFactory();
    }


    @Override
    public final String[] getDefaultCipherSuites() {
        return wrappedFactory.getDefaultCipherSuites();
    }

    @Override
    public final String[] getSupportedCipherSuites() {
        return wrappedFactory.getSupportedCipherSuites();
    }

    @Override
    public final Socket createSocket() throws IOException {
        return wrappedFactory.createSocket();
    }

    @Override
    public final Socket createSocket(final Socket socket, final String host, final int port,
                                     final boolean autoClose) throws IOException {
        return wrappedFactory.createSocket(socket, host, port, autoClose);
    }

    @Override
    public final Socket createSocket(final String host, final int port) throws IOException {
        return wrappedFactory.createSocket(host, port);
    }

    @Override
    public final Socket createSocket(final String host, final int port,
                                     final InetAddress localHost, final int localPort) throws IOException {
        return wrappedFactory.createSocket(host, port, localHost, localPort);
    }

    @Override
    public final Socket createSocket(final InetAddress host, final int port) throws IOException {
        return wrappedFactory.createSocket(host, port);
    }

    @Override
    public final Socket createSocket(final InetAddress address, final int port,
                                     final InetAddress localAddress, final int localPort) throws IOException {
        return wrappedFactory.createSocket(address, port, localAddress, localPort);
    }
}
