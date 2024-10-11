package io.github.greyp9.arwo.core.vm.mutex.test;

import org.junit.jupiter.api.Test;

import java.util.Random;
import java.util.logging.Logger;

public class Counters2Test {
    private final Random random = new Random(0L);
    private final Logger logger = Logger.getLogger(getClass().getName());

    private static final long PERIOD_REPORT = 1000L;

    @Test
    public void testCountersRecord() {
        final int iterations = 10;
        for (int i = 0; i < iterations; ++i) {
            final long begin = System.currentTimeMillis();
            callSDK();
            final long end = System.currentTimeMillis();
            recordSDK(end - begin, end);
        }
    }

    private long nextReport = System.currentTimeMillis() + PERIOD_REPORT;
    private long countCallsSDK = 0L;
    private long elapsedCallsSDK = 0L;

    private synchronized void recordSDK(final long elapsed, final long nowMillis) {
        ++countCallsSDK;
        elapsedCallsSDK += elapsed;
        if (nowMillis >= nextReport) {
            nextReport += PERIOD_REPORT;
            logger.info(String.format("INTERVAL, COUNT=%d, ELAPSED=%d", countCallsSDK, elapsedCallsSDK));
            countCallsSDK = 0L;
            elapsedCallsSDK = 0L;
        }
    }


    private void callSDK() {
        final int periodSleep = 20;
        try {
            Thread.sleep(1 + random.nextInt(periodSleep));
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
