package io.github.greyp9.arwo.core.time.test;

import io.github.greyp9.arwo.core.time.Stopwatch;
import org.junit.Assert;
import org.junit.Test;

public class StopwatchTest {

    @Test
    public void testSimple() throws Exception {
        final long aBitOfTime = 10L;
        final Stopwatch stopwatch = new Stopwatch(getClass().getSimpleName());
        stopwatch.lap();
        stopwatch.lap();
        long last = stopwatch.getLast();
        Assert.assertTrue(last < aBitOfTime);
        long[] laps = stopwatch.getLaps();
        Assert.assertEquals(2, laps.length);
        for (long lap : laps) {
            Assert.assertTrue(lap < aBitOfTime);
        }
    }
}
