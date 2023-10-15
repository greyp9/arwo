package io.github.greyp9.arwo.core.expr.test;

import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.net.URL;
import java.util.logging.Logger;

public class CodeSourceTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testGetCodeSource() {
        final URL location = getClass().getProtectionDomain().getCodeSource().getLocation();
        logger.info(location.toExternalForm());
        Assert.assertEquals("file", location.getProtocol());
        Assert.assertEquals("", location.getHost());
        Assert.assertTrue(new File(location.getFile()).exists());
    }
}
