package io.github.greyp9.arwo.core.url.test;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.URL;

public class UrlProtocolTest {

    @Test
    public void testProtocolHttps() throws Exception {
        final URL url = new URL("https://root@localhost:8443/hello/world/?a=1&b=2#ref");
        Assertions.assertEquals("https", url.getProtocol());
        Assertions.assertEquals("root@localhost:8443", url.getAuthority());
        Assertions.assertEquals("root", url.getUserInfo());
        Assertions.assertEquals("localhost", url.getHost());
        Assertions.assertEquals(PORT, url.getPort());
        Assertions.assertEquals("/hello/world/?a=1&b=2", url.getFile());
        Assertions.assertEquals("/hello/world/", url.getPath());
        Assertions.assertEquals("a=1&b=2", url.getQuery());
        Assertions.assertEquals("ref", url.getRef());
    }

    @Test
    public void testProtocolProxy() throws Exception {
        final URI uri = new URI("proxy://root@localhost:8443/");
        Assertions.assertEquals("proxy", uri.getScheme());
        Assertions.assertEquals("root@localhost:8443", uri.getAuthority());
        Assertions.assertEquals("root", uri.getUserInfo());
        Assertions.assertEquals("localhost", uri.getHost());
        Assertions.assertEquals(PORT, uri.getPort());
    }

    private static final int PORT = 8443;
}
