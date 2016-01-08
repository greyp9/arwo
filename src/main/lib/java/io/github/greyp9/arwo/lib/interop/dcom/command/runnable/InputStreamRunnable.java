package io.github.greyp9.arwo.lib.interop.dcom.command.runnable;

import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;
import io.github.greyp9.arwo.lib.interop.dcom.command.core.InputTextStream;
import org.jinterop.dcom.common.JIException;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

@SuppressWarnings("PMD.DoNotUseThreads")
public class InputStreamRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());
    private final Level level = Level.FINEST;
    private final long interval;

    private final InputTextStream stream;
    private final ByteBuffer byteBuffer;

    private boolean finished;

    public InputStreamRunnable(final InputTextStream stream, final ByteBuffer byteBuffer, final long interval) {
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
        logger.log(level, "START");
        try {
            runInner();
        } catch (JIException e) {
            logger.log(Level.SEVERE, e.getMessage(), e);
        } catch (IOException e) {
            logger.log(Level.SEVERE, e.getMessage(), e);
        }
        logger.log(level, "FINISH");
    }

    public final void runInner() throws IOException, JIException {
        // read until end of process signaled
        while (!finished) {
            readLine();
        }
        // read any remaining stream data
        while (!stream.atEndOfStream()) {
            readLine();
        }
        // teardown
        stream.getDispatch().release();
        MutexU.notifyAll(this);
    }

/*
    // works, but is slow
    private void readLineSlow() throws JIException, IOException {
        ThreadU.sleepMillis(interval);
        // read line content
        while (!stream.atEndOfLine()) {
            byteBuffer.addString(stream.read(1));
        }
        // read eol
        byteBuffer.addString(stream.read(2));
    }
*/

    private void readLine() throws JIException, IOException {
        ThreadU.sleepMillis(interval);
        byteBuffer.addString(stream.read(Const.CHUNK));
    }

    private static class Const {
        private static final int CHUNK = 64;
    }
}
