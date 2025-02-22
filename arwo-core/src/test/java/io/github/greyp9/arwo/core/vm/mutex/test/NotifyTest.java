package io.github.greyp9.arwo.core.vm.mutex.test;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.vm.exec.ExecutorServiceFactory;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;
import org.junit.jupiter.api.Test;

import java.util.Random;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

public class NotifyTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testWaitNotify() throws InterruptedException {
        final int iterations = 3;
        final CountDownLatch countDownLatch = new CountDownLatch(iterations);
        final Runnable runnable = new MyRunnable(countDownLatch);
        final ExecutorService executorService = ExecutorServiceFactory.create(1, "runner");
        logger.fine("executorService.submit()");
        executorService.submit(runnable);
        final boolean await = countDownLatch.await(iterations, TimeUnit.SECONDS);
        logger.fine(String.format("await() returned %s", await));
    }

    private static class MyRunnable implements Runnable {
        private final Logger logger = Logger.getLogger(getClass().getName());
        private final CountDownLatch countDownLatch;

        MyRunnable(final CountDownLatch countDownLatch) {
            this.countDownLatch = countDownLatch;
        }

        @Override
        public void run() {
            while (countDownLatch.getCount() > 0) {

                final int minWait = (int) DurationU.Const.HUNDRED_MILLIS;
                final int maxWait = (int) (DurationU.Const.ONE_SECOND_MILLIS / 2);
                ThreadU.sleepMillis((new Random().nextInt(maxWait - minWait)) + minWait);
                countDownLatch.countDown();
                logger.fine("countDown()");
            }
        }
    }

}
