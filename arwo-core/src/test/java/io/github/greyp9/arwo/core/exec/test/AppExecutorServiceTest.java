package io.github.greyp9.arwo.core.exec.test;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.exec.AppExecutorService;
import io.github.greyp9.arwo.core.exec.script.ScriptContext;
import io.github.greyp9.arwo.core.exec.script.ScriptRunnable;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.logging.Logger;

public class AppExecutorServiceTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private AppExecutorService executor;

    @BeforeEach
    final void setUp() {
        final File executorDir = new File(SystemU.tempDir(), getClass().getSimpleName());
        FileU.ensureFolder(executorDir);
        final int nThreads = 4;
        executor = new AppExecutorService(executorDir, nThreads);
        logger.info(String.format("Executor(%s)", executor.getPersistDir().getPath()));
    }

    @Test
    void testLifecycle() {
        final boolean stop = executor.shutdownNow(1);
        Assertions.assertTrue(stop);
        Assertions.assertTrue(executor.getFuturesIncomplete().isEmpty());
        Assertions.assertEquals(0, executor.getFutures().size());
    }

    @Test
    void testSleepRunnable() {
        final SleepRunnable sleepRunnable = new SleepRunnable(DurationU.Const.ONE_SECOND_MILLIS);
        executor.submit(sleepRunnable);
        ThreadU.sleepMillis(DurationU.Const.HUNDRED_MILLIS);  // give the job a chance to start
        final boolean stop = executor.shutdownNow(1);
        Assertions.assertTrue(stop);
        // ExecutorService interrupt short-circuits sleep
        Assertions.assertTrue(sleepRunnable.getElapsed() < DurationU.Const.ONE_SECOND_MILLIS);
        Assertions.assertTrue(executor.getFuturesIncomplete().isEmpty());
        Assertions.assertEquals(1, executor.getFutures().size());
    }

    private static class SleepRunnable implements Runnable {
        private final Logger logger = Logger.getLogger(getClass().getName());
        private final long millis;
        private long elapsed;

        SleepRunnable(final long millis) {
            this.millis = millis;
        }

        public long getElapsed() {
            return elapsed;
        }

        @Override
        public void run() {
            final long start = System.currentTimeMillis();
            logger.info("START");
            final boolean interrupted = ThreadU.sleepMillis(millis);
            logger.info(String.format("STOP (%s)", interrupted));
            this.elapsed = System.currentTimeMillis() - start;
        }
    }

    @Test
    void testScriptRunnable() throws IOException {
        final ScriptContext script = new ScriptContext("-", "ls", new Date(), executor.getPersistDir());
        final ScriptRunnable scriptRunnable = new ScriptRunnable(script, executor.getExecutorService());
        executor.submit(scriptRunnable);
        ThreadU.sleepMillis(DurationU.Const.HUNDRED_MILLIS);  // give the job a chance to start
        final boolean stop = executor.shutdownNow(1);
        Assertions.assertTrue(stop);

        final String stdout = script.getStdoutText();
        final String stderr = script.getStderrText();
        logger.info(String.format("STDOUT(%d): %s", stdout.length(), stdout));
        logger.info(String.format("STDERR(%d): %s", stderr.length(), stderr));
        Assertions.assertTrue(stdout.contains("pom.xml"));
        Assertions.assertTrue(stderr.isEmpty());
        Assertions.assertEquals(1, executor.getFutures().size());
        Assertions.assertTrue(executor.getFuturesIncomplete().isEmpty());
        Assertions.assertFalse(script.getDateStart().after(script.getDateFinish()));
        Assertions.assertEquals(0, script.getExitValue());
    }
}
