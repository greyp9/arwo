package io.github.greyp9.arwo.core.url.test;

import org.junit.Assert;
import org.junit.Test;

import java.net.URI;
import java.net.URL;

public class UrlProtocolTest {

    @Test
    public void testProtocolHttps() throws Exception {
        final URL url = new URL("https://root@localhost:8443/hello/world/?a=1&b=2#ref");
        Assert.assertEquals("https", url.getProtocol());
        Assert.assertEquals("root@localhost:8443", url.getAuthority());
        Assert.assertEquals("root", url.getUserInfo());
        Assert.assertEquals("localhost", url.getHost());
        Assert.assertEquals(8443, url.getPort());
        Assert.assertEquals("/hello/world/?a=1&b=2", url.getFile());
        Assert.assertEquals("/hello/world/", url.getPath());
        Assert.assertEquals("a=1&b=2", url.getQuery());
        Assert.assertEquals("ref", url.getRef());
    }

    @Test
    public void testProtocolProxy() throws Exception {
        final URI uri = new URI("proxy://root@localhost:8443/");
        Assert.assertEquals("proxy", uri.getScheme());
        Assert.assertEquals("root@localhost:8443", uri.getAuthority());
        Assert.assertEquals("root", uri.getUserInfo());
        Assert.assertEquals("localhost", uri.getHost());
        Assert.assertEquals(8443, uri.getPort());
    }
}
