package io.github.greyp9.arwo.core.time.test;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.time.Timer;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class TimerTest {

    @Test
    void testNotEnoughTime() {
        final Timer timer = new Timer(DurationU.Const.ONE_SECOND_MILLIS);
        Assertions.assertFalse(timer.check());
    }

    @Test
    void testEnoughTime() {
        final Timer timer = new Timer(DurationU.Const.TEN_MILLIS);
        ThreadU.sleepMillis(DurationU.Const.TEN_MILLIS * 2);
        Assertions.assertTrue(timer.check());
    }
}
