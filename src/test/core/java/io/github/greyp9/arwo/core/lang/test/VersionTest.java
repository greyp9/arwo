package io.github.greyp9.arwo.core.lang.test;

import io.github.greyp9.arwo.core.lang.VersionU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class VersionTest {

    @Test
    void testVersions() {
        Assertions.assertEquals(0, VersionU.languageLevel(""));
        Assertions.assertEquals(V8, VersionU.languageLevel("1.8.0_372"));
        Assertions.assertEquals(V11, VersionU.languageLevel("11.0.4"));
        Assertions.assertEquals(V17, VersionU.languageLevel("17.0.7"));
        Assertions.assertEquals(V21, VersionU.languageLevel("21"));
    }

    private static final int V8 = 8;
    private static final int V11 = 11;
    private static final int V17 = 17;
    private static final int V21 = 21;
}
