package io.github.greyp9.arwo.core.lang.test;

import io.github.greyp9.arwo.core.lang.MathU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

@SuppressWarnings("checkstyle:magicnumber")
public class MathTest {

    @Test
    public void testLogBase1_5() {
        final float f = 1.5f;
        Assertions.assertEquals(0, MathU.log(0L, f));
        Assertions.assertEquals(0, MathU.log(1L, f));
        Assertions.assertEquals(1, MathU.log(2L, f));
        Assertions.assertEquals(2, MathU.log(3L, f));
        Assertions.assertEquals(3, MathU.log(5L, f));
        Assertions.assertEquals(4, MathU.log(6L, f));
        Assertions.assertEquals(7, MathU.log(25L, f));
        Assertions.assertEquals(8, MathU.log(26L, f));
        Assertions.assertEquals(15, MathU.log(656L, f));
        Assertions.assertEquals(16, MathU.log(657L, f));
        Assertions.assertEquals(27, MathU.log(68_403L, f));
        Assertions.assertEquals(29, MathU.log(136_806L, f));
        Assertions.assertEquals(31, MathU.log(431_439L, f));
        Assertions.assertEquals(32, MathU.log(431_440L, f));
    }

    @Test
    public void testLogBase2() {
        Assertions.assertEquals(0, MathU.log(1L, 2));
        Assertions.assertEquals(1, MathU.log(2L, 2));
        Assertions.assertEquals(2, MathU.log(4L, 2));
        Assertions.assertEquals(3, MathU.log(8L, 2));
        Assertions.assertEquals(4, MathU.log(16L, 2));
        Assertions.assertEquals(5, MathU.log(32L, 2));
        Assertions.assertEquals(6, MathU.log(64L, 2));
        Assertions.assertEquals(7, MathU.log(128L, 2));

        Assertions.assertEquals(1, MathU.log(3L, 2));
        Assertions.assertEquals(2, MathU.log(7L, 2));
        Assertions.assertEquals(3, MathU.log(15L, 2));
        Assertions.assertEquals(4, MathU.log(31L, 2));
        Assertions.assertEquals(5, MathU.log(63L, 2));
        Assertions.assertEquals(6, MathU.log(127L, 2));
        Assertions.assertEquals(7, MathU.log(255L, 2));
    }

    @Test
    public void testLogBase3() {
        Assertions.assertEquals(0, MathU.log(1L, 3));
        Assertions.assertEquals(1, MathU.log(3L, 3));
        Assertions.assertEquals(2, MathU.log(9L, 3));
        Assertions.assertEquals(3, MathU.log(27L, 3));
        Assertions.assertEquals(4, MathU.log(81L, 3));
        Assertions.assertEquals(5, MathU.log(243L, 3));
        Assertions.assertEquals(6, MathU.log(729L, 3));
        Assertions.assertEquals(7, MathU.log(2187L, 3));

        Assertions.assertEquals(1, MathU.log(8L, 3));
        Assertions.assertEquals(2, MathU.log(26L, 3));
        Assertions.assertEquals(3, MathU.log(80L, 3));
        Assertions.assertEquals(4, MathU.log(242L, 3));
        Assertions.assertEquals(5, MathU.log(728L, 3));
        Assertions.assertEquals(6, MathU.log(2186L, 3));
        Assertions.assertEquals(7, MathU.log(6560L, 3));
    }
}
