package io.github.greyp9.arwo.core.exec.script;

import io.github.greyp9.arwo.core.io.runnable.InputStreamRunnable;
import io.github.greyp9.arwo.core.io.runnable.OutputStreamRunnable;
import io.github.greyp9.arwo.core.lang.ShellU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.vm.process.ProcessU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.concurrent.ExecutorService;
import java.util.logging.Logger;

public final class ScriptRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final ScriptContext scriptContext;
    private final ExecutorService executorServiceStreams;

    public ScriptRunnable(final ScriptContext scriptContext, final ExecutorService executorServiceStreams) {
        this.scriptContext = scriptContext;
        this.executorServiceStreams = executorServiceStreams;
    }

    @Override
    public void run() {
        scriptContext.setDateStart(new Date());
        final File dir = new File(SystemU.userDir());
        final String[] commandArray = ShellU.toCommandArray(scriptContext.getCommand());
        final Runtime runtime = Runtime.getRuntime();
        try {
            final Process process = runtime.exec(commandArray, null, dir);
            final Long processId = ProcessU.getProcessId(process);
            scriptContext.setPid(processId);
            logger.info(String.format("PID=%d", processId));
            final Integer exitValue = monitor(process);
            scriptContext.setExitValue(exitValue);
            scriptContext.setDateFinish(new Date());
            logger.info(String.format("PID=%d, EXIT_VALUE=%d", processId, exitValue));
            new ScriptSerializer().write(scriptContext);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private Integer monitor(final Process process) throws IOException {
        // monitor process streams (allow for supplemental process input)
        final OutputStreamRunnable runnableStdin = new OutputStreamRunnable(
                process.getOutputStream(), scriptContext.getStdin(), POLL_INTERVAL);
        final InputStreamRunnable runnableStdout = new InputStreamRunnable(
                process.getInputStream(), scriptContext.getStdout(), POLL_INTERVAL);
        final InputStreamRunnable runnableStderr = new InputStreamRunnable(
                process.getErrorStream(), scriptContext.getStderr(), POLL_INTERVAL);
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
            ThreadU.sleepMillis(POLL_INTERVAL);
            exitValue = isProcessFinished(process);
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

    private static final long POLL_INTERVAL = 10L;
}
