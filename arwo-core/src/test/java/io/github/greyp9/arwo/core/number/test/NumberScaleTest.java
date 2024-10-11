package io.github.greyp9.arwo.core.number.test;

import io.github.greyp9.arwo.core.number.NumberScale;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

@SuppressWarnings("checkstyle:magicnumber")
public class NumberScaleTest {

    @Test
    public void testSomeNumbers() throws Exception {
        Assertions.assertEquals("0", NumberScale.toString(0L));
        Assertions.assertEquals("1", NumberScale.toString(1L));
        Assertions.assertEquals("10", NumberScale.toString(10L));
        Assertions.assertEquals("100", NumberScale.toString(100L));
        Assertions.assertEquals("1000", NumberScale.toString(1000L));
        Assertions.assertEquals("1.00K", NumberScale.toString(1024L));
        Assertions.assertEquals("1.01K", NumberScale.toString(1035L));
        Assertions.assertEquals("1.10K", NumberScale.toString(1127L));
        Assertions.assertEquals("1.50K", NumberScale.toString(1536L));
        Assertions.assertEquals("9.76K", NumberScale.toString(10000L));
        Assertions.assertEquals("97.6K", NumberScale.toString(100000L));
        Assertions.assertEquals("976K", NumberScale.toString(1000000L));
        Assertions.assertEquals("9.53M", NumberScale.toString(10000000L));
        Assertions.assertEquals("95.3M", NumberScale.toString(100000000L));
        Assertions.assertEquals("953M", NumberScale.toString(1000000000L));
        Assertions.assertEquals("9.31G", NumberScale.toString(10000000000L));
        Assertions.assertEquals("93.1G", NumberScale.toString(100000000000L));
        Assertions.assertEquals("931G", NumberScale.toString(1000000000000L));
    }
}
