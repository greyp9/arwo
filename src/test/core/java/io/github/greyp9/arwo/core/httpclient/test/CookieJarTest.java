package io.github.greyp9.arwo.core.httpclient.test;

import io.github.greyp9.arwo.core.httpclient.CookieJar;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class CookieJarTest {

    @Test
    public void testUpdate() {
        final CookieJar cookieJar = new CookieJar();
        Assertions.assertEquals(0, cookieJar.getCookies().size());
        cookieJar.update("requestA", new NameTypeValues(new NameTypeValue("Set-Cookie", "a=1")));
        Assertions.assertEquals(1, cookieJar.getCookies().size());
        cookieJar.update("requestB", new NameTypeValues(
                new NameTypeValue("Set-Cookie", "a=11"),
                new NameTypeValue("Set-Cookie", "b=22")));
        Assertions.assertEquals(2, cookieJar.getCookies().size());
    }

    @Test
    public void testRegex() {
        final CookieJar cookieJar = new CookieJar();
        cookieJar.update("requestC", new NameTypeValues(
                new NameTypeValue("Set-Cookie", "aa=1"),
                new NameTypeValue("Set-Cookie", "bb=2"),
                new NameTypeValue("Set-Cookie", "c=3")));
        Assertions.assertEquals("aa=1; bb=2", cookieJar.toHeader("aa|bb"));
        Assertions.assertEquals("bb=2; c=3", cookieJar.toHeader("bb|c"));
        Assertions.assertEquals("aa=1; bb=2", cookieJar.toHeader("\\w{2}"));
   }
}
