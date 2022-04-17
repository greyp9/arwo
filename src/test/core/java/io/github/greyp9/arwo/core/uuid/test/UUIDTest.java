package io.github.greyp9.arwo.core.uuid.test;

import org.junit.Before;
import org.junit.Test;

import java.util.UUID;
import java.util.logging.Logger;

public class UUIDTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjustTiny(Logger.getLogger(""));
    }

    @Test
    public void testGenerateUUID() throws Exception {
        for (int i = 0; (i < 2); ++i) {
            logger.finest(UUID.randomUUID().toString());
        }
    }
}
