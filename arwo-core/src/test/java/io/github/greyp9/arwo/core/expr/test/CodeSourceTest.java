package io.github.greyp9.arwo.core.expr.test;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.net.URL;
import java.util.logging.Logger;

public class CodeSourceTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testGetCodeSource() {
        final URL location = getClass().getProtectionDomain().getCodeSource().getLocation();
        logger.info(location.toExternalForm());
        Assertions.assertEquals("file", location.getProtocol());
        Assertions.assertEquals("", location.getHost());
        Assertions.assertTrue(new File(location.getFile()).exists());
    }
}
