package io.github.greyp9.arwo.core.exec.script;

import io.github.greyp9.arwo.core.io.runnable.InputStreamRunnable;
import io.github.greyp9.arwo.core.io.runnable.OutputStreamRunnable;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicReference;

public final class ProcessMonitor {
    private final ScriptContext scriptContext;
    private final ExecutorService executorServiceStreams;
    private final AtomicReference<String> signal;
    private final long pollInterval;

    public ProcessMonitor(final ScriptContext scriptContext,
                          final ExecutorService executorServiceStreams,
                          final AtomicReference<String> signal,
                          final long pollInterval) {
        this.scriptContext = scriptContext;
        this.executorServiceStreams = executorServiceStreams;
        this.signal = signal;
        this.pollInterval = pollInterval;
    }

    public Integer monitor(final Process process) throws IOException {
        // monitor process streams (allow for supplemental process input)
        final OutputStreamRunnable runnableStdin = new OutputStreamRunnable(
                process.getOutputStream(), scriptContext.getStdin(), pollInterval);
        final InputStreamRunnable runnableStdout = new InputStreamRunnable(
                process.getInputStream(), scriptContext.getStdout(), pollInterval);
        final InputStreamRunnable runnableStderr = new InputStreamRunnable(
                process.getErrorStream(), scriptContext.getStderr(), pollInterval);
        final Runnable[] streams = { runnableStdin, runnableStdout, runnableStderr };
        for (final Runnable stream : streams) {
            executorServiceStreams.submit(stream);
        }
        return monitor(process, runnableStdin, runnableStdout, runnableStderr);
    }

    private Integer monitor(final Process process,
                            final OutputStreamRunnable runnableStdin,
                            final InputStreamRunnable runnableStdout,
                            final InputStreamRunnable runnableStderr) {
        Integer exitValue = null;
        while (exitValue == null) {
            ThreadU.sleepMillis(pollInterval);
            exitValue = isProcessFinished(process);
            if (signal.get() != null) {
                process.destroy();
            }
        }
        // allow process complete
        runnableStdin.stop();
        final InputStreamRunnable[] inputStreams = { runnableStdout, runnableStderr };
        for (final InputStreamRunnable stream : inputStreams) {
            stream.waitForComplete();
        }
        return exitValue;
    }

    private static Integer isProcessFinished(final Process process) {
        try {
            return process.exitValue();
        } catch (IllegalThreadStateException e) {
            return null;
        }
    }
}
