package io.github.greyp9.arwo.core.input.runnable;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

@SuppressWarnings({ "PMD.DoNotUseThreads", "PMD.AvoidFinalLocalVariable" })
public class InputStreamRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final InputStream inputStream;
    private final AtomicReference<String> reference;
    private final ByteArrayOutputStream outputStream;
    private final long interval;

    public InputStreamRunnable(
            final InputStream inputStream, final AtomicReference<String> reference, final long interval) {
        this.inputStream = inputStream;
        this.reference = reference;
        this.outputStream = new ByteArrayOutputStream();
        this.interval = interval;
    }

    @Override
    public final void run() {
        final String methodName = "run()";  // i18n trace
        logger.entering(getClass().getSimpleName(), methodName);

        while (reference.get() == null) {
            monitor();
        }
        MutexU.notifyAll(reference);
        logger.exiting(getClass().getSimpleName(), methodName);
    }

    private byte[] getInput() {
        synchronized (outputStream) {
            final byte[] bytes = outputStream.toByteArray();
            outputStream.reset();
            return ((bytes.length == 0) ? null : bytes);
        }
    }

    private void monitor() {
        try {
            synchronized (outputStream) {
                while (inputStream.available() > 0) {
                    outputStream.write(inputStream.read());
                }
                if (outputStream.size() > 0) {
                    reference.compareAndSet(null, UTF8Codec.toString(getInput()).trim());
                }
            }
            if (reference.get() == null) {
                ThreadU.sleepMillis(interval);
            }
        } catch (IOException e) {
            reference.compareAndSet(null, e.getMessage());
        }
    }
}
