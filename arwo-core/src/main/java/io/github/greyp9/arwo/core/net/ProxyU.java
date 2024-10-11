package io.github.greyp9.arwo.core.net;

import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.URL;

public final class ProxyU {

    private ProxyU() {
    }

    public static Proxy toHttpProxy(final URL url) {
        return (url == null) ? null : toHttpProxyNN(url);
    }

    public static Proxy toHttpProxyNN(final URL url) {
        final InetSocketAddress address = new InetSocketAddress(url.getHost(), url.getPort());
        return new Proxy(Proxy.Type.HTTP, address);
    }
}
