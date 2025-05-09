package io.github.greyp9.arwo.core.io.runnable;

import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;

import java.io.IOException;
import java.io.OutputStream;
import java.util.logging.Logger;

@SuppressWarnings({ "PMD.DoNotUseThreads", "PMD.AvoidSynchronizedAtMethodLevel" })
public class OutputStreamRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());
    private final OutputStream os;
    private final ByteBuffer byteBuffer;
    private final long pollInterval;

    private boolean stopped;
    private Exception exception;

    public OutputStreamRunnable(final OutputStream os, final ByteBuffer byteBuffer, final long pollInterval) {
        this.os = os;
        this.byteBuffer = byteBuffer;
        this.pollInterval = pollInterval;
        this.stopped = false;
        this.exception = null;
    }

    public final synchronized boolean isStopped() {
        return stopped;
    }

    public final synchronized void stop() {
        this.stopped = true;
    }

/*
    public synchronized void waitForComplete() {
        stop();
        MutexU.wait(this, COMPLETE_INTERVAL_MILLIS);
    }
*/

    public final synchronized Exception getException() {
        return exception;
    }

    private synchronized void setException(final Exception exception) {
        this.exception = exception;
    }

    @Override
    public final void run() {
        final String threadName = Thread.currentThread().getName();
        logger.entering(getClass().getName(), threadName);
        try {
            runInner();
        } catch (IOException e) {
            setException(e);
        } finally {
            logger.exiting(getClass().getName(), threadName);
        }
    }

    public final void runInner() throws IOException {
        while (!isStopped()) {
            ThreadU.sleepMillis(pollInterval);
            write();
        }
        MutexU.notifyAll(this);
    }

    private void write() throws IOException {
        final int length = byteBuffer.getLength();
        if (length > 0) {
            final byte[] bytes = byteBuffer.getBytes(true);
            os.write(bytes);
            os.flush();
            MutexU.notifyAll(this);
        }
    }
}
