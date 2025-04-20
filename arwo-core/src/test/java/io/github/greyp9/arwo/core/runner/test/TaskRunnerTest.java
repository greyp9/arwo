package io.github.greyp9.arwo.core.runner.test;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.runner.Command;
import io.github.greyp9.arwo.core.runner.CommandTask;
import io.github.greyp9.arwo.core.runner.TaskRunner;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

public class TaskRunnerTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private static final int N_THREADS = 4;  // one for process thread, three for process stream threads
    private static final long POLL_INTERVAL = DurationU.Const.HUNDRED_MILLIS;

    @Test
    void testSimple() {
        final TaskRunner taskRunner = new TaskRunner(getClass().getSimpleName(), N_THREADS, POLL_INTERVAL);
        final CommandTask task = taskRunner.submit(new Command("ls"));  // working dir = 'arwo/arwo-core'
        logger.fine(String.format("AWAIT=%s", task.getCommand().getInput()));
        final boolean await = task.await(1, TimeUnit.MINUTES);
        final String stdout = task.getStdout();
        final String stderr = task.getStderr();
        logger.fine(String.format("COMPLETE=%s, STDOUT=%d, STDERR=%d", await, stdout.length(), stderr.length()));
        Assertions.assertTrue(await);
        Assertions.assertEquals(0, task.getExitValue());
        Assertions.assertNull(task.getException());
        Assertions.assertFalse(task.getStdout().isEmpty());
        Assertions.assertTrue(task.getStdout().contains("pom.xml"));
        Assertions.assertTrue(task.getStderr().isEmpty());
    }

    @Test
    void testMultipleCommands() {
        final TaskRunner taskRunner = new TaskRunner(getClass().getSimpleName(), N_THREADS * 2, POLL_INTERVAL);
        final CommandTask task1 = taskRunner.submit(new Command("ls"));  // working dir = 'arwo/arwo-core'
        final CommandTask task2 = taskRunner.submit(new Command("ls -la"));  // working dir = 'arwo/arwo-core'
        final CommandTask[] tasks = { task1, task2 };
        for (CommandTask task : tasks) {
            logger.fine(String.format("AWAIT=%s", task.getCommand().getInput()));
            final boolean await = task.await(1, TimeUnit.MINUTES);
            final String stdin = task.getCommand().getInput();
            final String stdout = task.getStdout();
            final String stderr = task.getStderr();
            logger.fine(String.format("COMPLETE=%s, STDIN=%s, STDOUT=%d, STDERR=%d",
                    await, stdin, stdout.length(), stderr.length()));
            Assertions.assertTrue(await);
            Assertions.assertEquals(0, task.getExitValue());
            Assertions.assertNull(task.getException());
            Assertions.assertFalse(task.getStdout().isEmpty());
            Assertions.assertTrue(task.getStdout().contains("pom.xml"));
            Assertions.assertTrue(task.getStderr().isEmpty());
        }
        Assertions.assertTrue(task1.getStdout().length() < task2.getStdout().length());
    }

    @Test
    void testMultipleCommandsInsufficientThreads() {
        final TaskRunner taskRunner = new TaskRunner(getClass().getSimpleName(), N_THREADS - 1, POLL_INTERVAL);
        final CommandTask task = taskRunner.submit(new Command("ls"));  // working dir = 'arwo/arwo-core'
        logger.fine(String.format("AWAIT=%s", task.getCommand().getInput()));
        final boolean await = task.await(1, TimeUnit.MINUTES);
        final String stdin = task.getCommand().getInput();
        final String stdout = task.getStdout();
        final String stderr = task.getStderr();
        logger.fine(String.format("COMPLETE=%s, STDIN=%s, STDOUT=%d, STDERR=%d",
                await, stdin, stdout.length(), stderr.length()));
        Assertions.assertTrue(await);
        Assertions.assertNull(task.getExitValue());
        Assertions.assertInstanceOf(IOException.class, task.getException());
        Assertions.assertTrue(task.getStdout().isEmpty());
        Assertions.assertTrue(task.getStderr().isEmpty());
    }

    @Test
    void testAdditionalStdin() {
        final TaskRunner taskRunner = new TaskRunner(getClass().getSimpleName(), N_THREADS, POLL_INTERVAL);
        final CommandTask task = taskRunner.submit(new Command("read LABEL"));  // waits for input
        logger.fine(String.format("AWAIT=%s", task.getCommand().getInput()));
        final boolean await = task.await(1, TimeUnit.SECONDS);
        Assertions.assertFalse(await);
        final boolean added = task.addStdin("hello");
        Assertions.assertTrue(added);
        final boolean await2 = task.await(1, TimeUnit.SECONDS);
        final String stdout = task.getStdout();
        final String stderr = task.getStderr();
        logger.fine(String.format("COMPLETE=%s, STDOUT=%d, STDERR=%d", await2, stdout.length(), stderr.length()));
        Assertions.assertTrue(await2);
        Assertions.assertEquals(0, task.getExitValue());
        Assertions.assertNull(task.getException());
        Assertions.assertTrue(task.getStdout().isEmpty());
        Assertions.assertTrue(task.getStderr().isEmpty());
    }
}
