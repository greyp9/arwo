package io.github.greyp9.arwo.core.runner;

import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.io.runnable.InputStreamRunnable;
import io.github.greyp9.arwo.core.io.runnable.OutputStreamRunnable;
import io.github.greyp9.arwo.core.lang.ShellU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.vm.exec.ThreadPoolU;
import io.github.greyp9.arwo.core.vm.process.ProcessU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Logger;

public final class CommandTask implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final ExecutorService executor;
    private final Command command;
    private final long pollInterval;
    private final CountDownLatch completeLatch;

    private Long pid;
    private Integer exitValue;
    private IOException exception;

    public CommandTask(final ExecutorService executor, final Command command, final long pollInterval) {
        this.executor = executor;
        this.command = command;
        this.pollInterval = pollInterval;
        this.completeLatch = new CountDownLatch(1);  // await process complete, stream threads complete
    }

    public Command getCommand() {
        return command;
    }

    public Long getPid() {
        return pid;
    }

    public boolean await(final long timeout, final TimeUnit timeUnit) {
        try {
            return completeLatch.await(timeout, timeUnit);
        } catch (InterruptedException e) {
            return false;
        }
    }

    public Integer getExitValue() {
        return exitValue;
    }

    public IOException getException() {
        return exception;
    }

    public boolean addStdin(final String input) {
        return command.getStdin().addString(input + SystemU.eol());
    }

    public String getStdout() {
        return getText(command.getStdout());
    }

    public String getStderr() {
        return getText(command.getStderr());
    }

    public String getText(final ByteBuffer byteBuffer) {
        try {
            return new String(byteBuffer.getBytes(), byteBuffer.getCharset());
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    @Override
    public void run() {
        final String threadName = Thread.currentThread().getName();
        logger.entering(getClass().getName(), threadName);
        final String[] commandArray = ShellU.toCommandArray(command.getInput());
        final Runtime runtime = Runtime.getRuntime();
        try {
            final int streamThreadsNeeded = 3;  // bail if insufficient threads for process streams
            if (ThreadPoolU.isAvailablePool(executor, streamThreadsNeeded)) {
                final Process process = runtime.exec(commandArray, null, command.getDir());
                this.pid = ProcessU.getProcessId(process);
                this.exitValue = monitor(process);
            } else {
                throw new IOException("insufficient threads");
            }
        } catch (IOException e) {
            this.exception = e;
        } finally {
            completeLatch.countDown();
            logger.exiting(getClass().getName(), threadName);
        }
    }

    private Integer monitor(final Process process) {
        final InputStreamRunnable runnableStdout = new InputStreamRunnable(
                process.getInputStream(), command.getStdout(), pollInterval);
        final InputStreamRunnable runnableStderr = new InputStreamRunnable(
                process.getErrorStream(), command.getStderr(), pollInterval);
        // allow for supplemental process input
        final OutputStreamRunnable runnableStdin = new OutputStreamRunnable(
                process.getOutputStream(), command.getStdin(), pollInterval);
        final Runnable[] streams = { runnableStdout, runnableStderr, runnableStdin };
        final List<Future<?>> futures = new ArrayList<>();
        for (final Runnable stream : streams) {
            futures.add(executor.submit(stream));
        }
        // monitor process
        Integer exitValueProcess = null;
        while (exitValueProcess == null) {
            ThreadU.sleepMillis(pollInterval);
            exitValueProcess = ProcessU.isProcessFinished(process);
        }
        // allow stream writes to complete
        runnableStdin.stop();
        runnableStdout.stop();
        runnableStderr.stop();
        for (Future<?> future : futures) {
            try {
                future.get(1, TimeUnit.MINUTES);
            } catch (InterruptedException | ExecutionException | TimeoutException e) {
                command.getStderr().addString(e.getMessage());
            }
        }
        return exitValueProcess;
    }
}
