package io.github.greyp9.arwo.core.res.test;

import io.github.greyp9.arwo.core.res.ResourceU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URL;
import java.util.logging.Logger;

public class ResourceTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testResource() throws IOException {
        final URL urlFile = ResourceU.resolve("io/github/greyp9/arwo/core/value/Value.class");
        Assertions.assertNotNull(urlFile);
        logger.info(urlFile.toExternalForm());

        final URL urlJar = ResourceU.resolve("java/lang/String.class");
        Assertions.assertNotNull(urlJar);
        logger.info(urlJar.toExternalForm());
    }
}
