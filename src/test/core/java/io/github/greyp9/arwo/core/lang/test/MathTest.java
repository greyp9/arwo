package io.github.greyp9.arwo.core.lang.test;

import io.github.greyp9.arwo.core.lang.MathU;
import org.junit.Assert;
import org.junit.Test;

public class MathTest {

    @Test
    public void testLogBase2() throws Exception {
        Assert.assertEquals(0, MathU.log(1L, 2));
        Assert.assertEquals(1, MathU.log(2L, 2));
        Assert.assertEquals(2, MathU.log(4L, 2));
        Assert.assertEquals(3, MathU.log(8L, 2));
        Assert.assertEquals(4, MathU.log(16L, 2));
        Assert.assertEquals(5, MathU.log(32L, 2));
        Assert.assertEquals(6, MathU.log(64L, 2));
        Assert.assertEquals(7, MathU.log(128L, 2));

        Assert.assertEquals(1, MathU.log(3L, 2));
        Assert.assertEquals(2, MathU.log(7L, 2));
        Assert.assertEquals(3, MathU.log(15L, 2));
        Assert.assertEquals(4, MathU.log(31L, 2));
        Assert.assertEquals(5, MathU.log(63L, 2));
        Assert.assertEquals(6, MathU.log(127L, 2));
        Assert.assertEquals(7, MathU.log(255L, 2));
    }

    @Test
    public void testLogBase3() throws Exception {
        Assert.assertEquals(0, MathU.log(1L, 3));
        Assert.assertEquals(1, MathU.log(3L, 3));
        Assert.assertEquals(2, MathU.log(9L, 3));
        Assert.assertEquals(3, MathU.log(27L, 3));
        Assert.assertEquals(4, MathU.log(81L, 3));
        Assert.assertEquals(5, MathU.log(243L, 3));
        Assert.assertEquals(6, MathU.log(729L, 3));
        Assert.assertEquals(7, MathU.log(2187L, 3));

        Assert.assertEquals(1, MathU.log(8L, 3));
        Assert.assertEquals(2, MathU.log(26L, 3));
        Assert.assertEquals(3, MathU.log(80L, 3));
        Assert.assertEquals(4, MathU.log(242L, 3));
        Assert.assertEquals(5, MathU.log(728L, 3));
        Assert.assertEquals(6, MathU.log(2186L, 3));
        Assert.assertEquals(7, MathU.log(6560L, 3));
    }
}
