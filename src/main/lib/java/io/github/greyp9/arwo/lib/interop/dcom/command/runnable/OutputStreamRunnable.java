package io.github.greyp9.arwo.lib.interop.dcom.command.runnable;

import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;
import io.github.greyp9.arwo.lib.interop.dcom.command.core.OutputTextStream;
import org.jinterop.dcom.common.JIException;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

@SuppressWarnings("PMD.DoNotUseThreads")
public class OutputStreamRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());
    private final long interval;

    private final OutputTextStream stream;
    private final ByteBuffer byteBuffer;

    private boolean finished;

    public OutputStreamRunnable(final OutputTextStream stream, final ByteBuffer byteBuffer, final long interval) {
        this.stream = stream;
        this.byteBuffer = byteBuffer;
        this.interval = interval;
        this.finished = false;
    }

    public final void setFinished() {
        this.finished = true;
    }

    @Override
    public final void run() {
        logger.entering(getClass().getName(), Runnable.class.getName());
        try {
            runInner();
        } catch (JIException e) {
            logger.log(Level.SEVERE, e.getMessage(), e);
        } catch (IOException e) {
            logger.log(Level.SEVERE, e.getMessage(), e);
        } finally {
            logger.exiting(getClass().getName(), Runnable.class.getName());
        }
    }

    public final void runInner() throws IOException, JIException {
        // run until end of process signaled
        while (!finished) {
            write();
        }
        // teardown
        stream.getDispatch().release();
        MutexU.notifyAll(this);
    }

    private void write() throws IOException, JIException {
        ThreadU.sleepMillis(interval);
        final int length = byteBuffer.getLength();
        if (length > 0) {
            final byte[] bytes = byteBuffer.getBytes(true);
            stream.write(new String(bytes, byteBuffer.getCharset()));
        }
    }
}
