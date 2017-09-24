package io.github.greyp9.arwo.core.uuid.test;

import junit.framework.TestCase;

import java.util.UUID;
import java.util.logging.Logger;

public class UUIDTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjustTiny(Logger.getLogger(""));
    }

    public void testGenerateUUID() throws Exception {
        for (int i = 0; (i < 6); ++i) {
            logger.info(UUID.randomUUID().toString());
        }
    }
}
