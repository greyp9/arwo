package io.github.greyp9.arwo.core.input.runnable;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

public class InputRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final InputStream inputStream;
    private final AtomicReference<String> reference;
    private final List<String> input;
    private final long interval;

    public InputRunnable(final InputStream inputStream,
                         final AtomicReference<String> reference,
                         final List<String> input,
                         final long interval) {
        this.inputStream = inputStream;
        this.reference = reference;
        this.input = input;
        this.interval = interval;
    }

    @Override
    public final void run() {
        final String methodName = "run()";  // i18n trace
        logger.entering(getClass().getSimpleName(), methodName);
        while (reference.get() == null) {
            try {
                while (inputStream.available() > 0) {
                    input.add(UTF8Codec.toString(StreamU.read(inputStream, inputStream.available())));
                }
                if (!input.isEmpty()) {
                    MutexU.notifyAll(reference);
                }
                if (reference.get() == null) {
                    MutexU.wait(reference, interval);
                }
            } catch (IOException e) {
                reference.compareAndSet(null, e.getMessage());
            }
        }
        logger.exiting(getClass().getSimpleName(), methodName);
    }
}
