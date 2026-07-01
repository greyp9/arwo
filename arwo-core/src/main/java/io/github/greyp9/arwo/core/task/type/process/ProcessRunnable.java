package io.github.greyp9.arwo.core.task.type.process;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.vm.env.EnvironmentU;
import io.github.greyp9.arwo.core.vm.mutex.MapU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Date;
import java.util.HashMap;
import java.util.logging.Logger;

public class ProcessRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final ProcessTask task;

    public ProcessRunnable(final ProcessTask task) {
        this.task = task;
    }

    @Override
    public final void run() {
        final Runtime runtime = Runtime.getRuntime();
        final String[] envp = EnvironmentU.toEnvP(MapU.join(new HashMap<>(), System.getenv(), task.getEnv()));
        try {
            task.setDateStart(new Date());
            final Process process = (task.getCmd1() != null)
                    ? runtime.exec(task.getCmd1(), envp, task.getDir())
                    : runtime.exec(task.getCmd(), envp, task.getDir());
            final InputStream stdout = new BufferedInputStream(process.getInputStream());
            final InputStream stderr = new BufferedInputStream(process.getErrorStream());
            final ByteBuffer byteBufferStdout = task.getStdout();
            final ByteBuffer byteBufferStderr = task.getStderr();

            Integer exitValue = null;
            while (exitValue == null) {
                ThreadU.sleepMillis(DurationU.Const.TEN_MILLIS);
                exitValue = isProcessFinished(process);
                byteBufferStdout.addBytes(StreamU.read(stdout, stdout.available()));
                byteBufferStderr.addBytes(StreamU.read(stderr, stderr.available()));
            }
            task.setDateFinish(new Date());
            task.setExitValue(exitValue);
            byteBufferStdout.addBytes(StreamU.read(stdout, stdout.available()));
            byteBufferStderr.addBytes(StreamU.read(stderr, stderr.available()));
        } catch (IOException e) {
            logger.throwing(getClass().getName(), "run()", e);
            throw new RuntimeException(e);
        }
    }

    private static Integer isProcessFinished(final Process process) {
        try {
            return process.exitValue();
        } catch (IllegalThreadStateException e) {
            return null;
        }
    }
}
