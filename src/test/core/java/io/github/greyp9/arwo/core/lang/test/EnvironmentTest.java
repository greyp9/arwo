package io.github.greyp9.arwo.core.lang.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.text.FingerPrint;
import io.github.greyp9.arwo.core.vm.env.EnvironmentU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.Collections;
import java.util.logging.Logger;

public class EnvironmentTest {

    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testGetEnv() throws IOException {
        final byte[] sysenv = EnvironmentU.getEnv(Collections.<String>emptyList());
        Assertions.assertNotNull(sysenv);
        logger.finest(UTF8Codec.toString(sysenv));
        logger.finest(FingerPrint.toHex256(sysenv));
    }
}
