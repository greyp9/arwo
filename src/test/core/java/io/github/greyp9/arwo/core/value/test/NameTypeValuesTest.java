package io.github.greyp9.arwo.core.value.test;

import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.httpclient.HttpClientU;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.logging.Logger;

public class NameTypeValuesTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testUseForHeaders() {
        final String basicAuth = HttpClientU.toBasicAuth("test", "test".toCharArray());
        final NameTypeValues headers = NTV.create(
                Http.Header.HOST, "localhost:8443",
                Http.Header.USER_AGENT, getClass().getSimpleName(),
                Http.Header.ACCEPT, Http.Mime.TEXT_HTML_UTF8,
                Http.Header.ACCEPT_ENCODING, Http.Header.GZIP,
                Http.Header.AUTHORIZATION, basicAuth);
        logger.finest(headers.toString());
    }

    @Test
    public void testUseForQueryString() throws Exception {
        final String queryString1 = HttpArguments.toQueryString(new NameTypeValues()
                .addNN("a", null).addNN("b", null));
        Assertions.assertEquals("", queryString1);

        final String queryString2 = HttpArguments.toQueryString(new NameTypeValues()
                .addNN("a", "foo").addNN("b", null));
        Assertions.assertEquals("a=foo", queryString2);

        final String queryString3 = HttpArguments.toQueryString(new NameTypeValues()
                .addNN("a", "foo").addNN("b", "bar"));
        Assertions.assertEquals("a=foo&b=bar", queryString3);

        final String queryString4 = HttpArguments.toQueryString(new NameTypeValues()
                .addNN("a", null).addNN("b", "bar"));
        Assertions.assertEquals("b=bar", queryString4);
    }
}
