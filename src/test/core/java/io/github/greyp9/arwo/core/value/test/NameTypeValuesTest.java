package io.github.greyp9.arwo.core.value.test;

import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.httpclient.HttpClientU;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import org.junit.Before;
import org.junit.Test;

import java.util.logging.Logger;

public class NameTypeValuesTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testUseForHeaders() throws Exception {
        final String basicAuth = HttpClientU.toBasicAuth("test", "test".toCharArray());
        final NameTypeValues headers = NTV.create(
                Http.Header.HOST, "localhost:8443",
                Http.Header.USER_AGENT, getClass().getSimpleName(),
                Http.Header.ACCEPT, Http.Mime.TEXT_HTML_UTF8,
                Http.Header.ACCEPT_ENCODING, Http.Header.GZIP,
                Http.Header.AUTHORIZATION, basicAuth);
        logger.info(headers.toString());
    }
}
