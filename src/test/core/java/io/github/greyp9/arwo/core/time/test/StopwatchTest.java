package io.github.greyp9.arwo.core.time.test;

import io.github.greyp9.arwo.core.time.Stopwatch;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class StopwatchTest {

    @Test
    public void testSimple() throws Exception {
        final long aBitOfTime = 10L;
        final Stopwatch stopwatch = new Stopwatch(getClass().getSimpleName());
        stopwatch.lap();
        stopwatch.lap();
        long last = stopwatch.getLast();
        Assertions.assertTrue(last < aBitOfTime);
        long[] laps = stopwatch.getLaps();
        Assertions.assertEquals(2, laps.length);
        for (long lap : laps) {
            Assertions.assertTrue(lap < aBitOfTime);
        }
    }
}
