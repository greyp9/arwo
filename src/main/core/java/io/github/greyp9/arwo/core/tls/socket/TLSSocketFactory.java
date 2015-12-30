package io.github.greyp9.arwo.core.tls.socket;

import javax.net.SocketFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.security.GeneralSecurityException;

public class TLSSocketFactory extends SSLSocketFactory {
    private static SSLSocketFactory wrappedFactory;

    public static synchronized void initialize(SSLContext sslContext) throws GeneralSecurityException {
        wrappedFactory = sslContext.getSocketFactory();
    }

    public static synchronized SocketFactory getDefault() {
        return new TLSSocketFactory();
    }


    @Override
    public String[] getDefaultCipherSuites() {
        return wrappedFactory.getDefaultCipherSuites();
    }

    @Override
    public String[] getSupportedCipherSuites() {
        return wrappedFactory.getSupportedCipherSuites();
    }

    @Override
    public Socket createSocket() throws IOException {
        return wrappedFactory.createSocket();
    }

    @Override
    public Socket createSocket(Socket socket, String host, int port, boolean autoClose) throws IOException {
        return wrappedFactory.createSocket(socket, host, port, autoClose);
    }

    @Override
    public Socket createSocket(String host, int port) throws IOException {
        return wrappedFactory.createSocket(host, port);
    }

    @Override
    public Socket createSocket(String host, int port, InetAddress localHost, int localPort) throws IOException {
        return wrappedFactory.createSocket(host, port, localHost, localPort);
    }

    @Override
    public Socket createSocket(InetAddress host, int port) throws IOException {
        return wrappedFactory.createSocket(host, port);
    }

    @Override
    public Socket createSocket(
            InetAddress address, int port, InetAddress localAddress, int localPort) throws IOException {
        return wrappedFactory.createSocket(address, port, localAddress, localPort);
    }
}
