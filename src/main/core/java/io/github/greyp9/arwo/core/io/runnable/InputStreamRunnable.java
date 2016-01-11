package io.github.greyp9.arwo.core.io.runnable;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;

import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Logger;

@SuppressWarnings({ "PMD.DoNotUseThreads", "PMD.AvoidSynchronizedAtMethodLevel" })
public class InputStreamRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());
    private final InputStream is;
    private final ByteBuffer byteBuffer;
    private final long pollInterval;

    private boolean stopped;
    private Exception exception;

    public final long getPollInterval() {
        return pollInterval;
    }

    public InputStreamRunnable(final InputStream is, final ByteBuffer byteBuffer, final long pollInterval) {
        this.is = is;
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

    public final synchronized void waitForComplete() {
        stop();
        MutexU.wait(this, DurationU.toMillis("PT5S"));
    }

    public final synchronized Exception getException() {
        return exception;
    }

    private synchronized void setException(final Exception exception) {
        this.exception = exception;
    }

/*
    public synchronized int getByteCount() throws IOException {
        return byteBuffer.getLength();
    }
*/

    public final synchronized byte[] getBytes() throws IOException {
        return byteBuffer.getBytes();
    }

/*
    public synchronized byte[] getLastBytes(int count) throws IOException {
        byte[] bytesAll = byteBuffer.getBytes();
        int start = Math.max((bytesAll.length - count), 0);
        if ((count == 0) || (start == 0)) {
            return bytesAll;
        } else {
            byte[] bytes = new byte[count];
            System.arraycopy(bytesAll, start, bytes, 0, count);
            return bytes;
        }
    }
*/

    @Override
    public final void run() {
        logger.entering(getClass().getName(), Runnable.class.getName());
        try {
            runInner();
        } catch (IOException e) {
            setException(e);
        } finally {
            logger.exiting(getClass().getName(), Runnable.class.getName());
        }
    }

    public final void runInner() throws IOException {
        while (!isStopped()) {
            ThreadU.sleepMillis(pollInterval);
            read();
        }
        MutexU.notifyAll(this);
    }

    @SuppressWarnings("PMD.AssignmentInOperand")
    private void read() throws IOException {
        int available;
        while ((available = is.available()) > 0) {
            available = Math.min(available, Const.BLOCK_READ_BYTES);
            final byte[] bytes = StreamU.read(is, available);
            //java.util.logging.Logger.getLogger(getClass().getName()).info("" + bytes.length);
            byteBuffer.addBytes(bytes);
            MutexU.notifyAll(this);
        }
    }

    private static class Const {
        public static final int BLOCK_READ_BYTES = 16384;
    }
}
