package io.github.greyp9.arwo.core.number.test;

import io.github.greyp9.arwo.core.number.NumberScale;
import org.junit.Assert;
import org.junit.Test;

public class NumberScaleTest {

    @Test
    public void testSomeNumbers() throws Exception {
        Assert.assertEquals("0", NumberScale.toString(0L));
        Assert.assertEquals("1", NumberScale.toString(1L));
        Assert.assertEquals("10", NumberScale.toString(10L));
        Assert.assertEquals("100", NumberScale.toString(100L));
        Assert.assertEquals("1000", NumberScale.toString(1000L));
        Assert.assertEquals("1.00K", NumberScale.toString(1024L));
        Assert.assertEquals("1.01K", NumberScale.toString(1035L));
        Assert.assertEquals("1.10K", NumberScale.toString(1127L));
        Assert.assertEquals("1.50K", NumberScale.toString(1536L));
        Assert.assertEquals("9.76K", NumberScale.toString(10000L));
        Assert.assertEquals("97.6K", NumberScale.toString(100000L));
        Assert.assertEquals("976K", NumberScale.toString(1000000L));
        Assert.assertEquals("9.53M", NumberScale.toString(10000000L));
        Assert.assertEquals("95.3M", NumberScale.toString(100000000L));
        Assert.assertEquals("953M", NumberScale.toString(1000000000L));
        Assert.assertEquals("9.31G", NumberScale.toString(10000000000L));
        Assert.assertEquals("93.1G", NumberScale.toString(100000000000L));
        Assert.assertEquals("931G", NumberScale.toString(1000000000000L));
    }
}
