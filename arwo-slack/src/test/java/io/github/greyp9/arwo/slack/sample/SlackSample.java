package io.github.greyp9.arwo.slack.sample;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.input.runnable.InputRunnable;
import io.github.greyp9.arwo.core.vm.exec.ExecutorServiceFactory;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicReference;

public final class SlackSample {
    private final Logger logger = LoggerFactory.getLogger(getClass());

    public void run() {
        logger.info("BEGIN");
        final String botToken = System.getenv("SLACK_BOT_TOKEN");
        final String channelId = System.getenv("SLACK_CHANNEL");

        final AtomicReference<String> reference = new AtomicReference<>();
        final List<String> input = Collections.synchronizedList(new ArrayList<>());
        final List<String> output = Collections.synchronizedList(new ArrayList<>());
        final int threadCount = 3;  // InputRunnable, SlackReaderRunnable, SlackWriterRunnable
        final String threadPrefix = getClass().getSimpleName();
        final ExecutorService executorService = ExecutorServiceFactory.create(threadCount, threadPrefix);
        executorService.execute(new InputRunnable(System.in, reference, input, INTERVAL));
        executorService.execute(new SlackReaderRunnable(reference, botToken, channelId, "PT12S"));
        executorService.execute(new SlackWriterRunnable(reference, botToken, channelId, output, INTERVAL));
        while (reference.get() == null) {
            MutexU.wait(input, INTERVAL);  // input monitor should wait first
            processInput(reference, input, output);
        }
        executorService.shutdown();
        logger.info("END: {}", reference.get());
    }

    private void processInput(final AtomicReference<String> reference,
                              final List<String> input,
                              final List<String> output) {
        while (!input.isEmpty()) {
            final String input1 = input.remove(0).trim();
            logger.info("INPUT: [{}]", input1);
            if (input1.contains("q")) {
                reference.compareAndSet(null, input1);
                MutexU.notifyAll(reference);
            } else if (!input1.isEmpty()) {
                output.add(input1);
            }
        }
    }

    public static void main(final String[] args) {
        new SlackSample().run();
        System.exit(0);
    }

    public static final long INTERVAL = DurationU.Const.ONE_SECOND_MILLIS / 2;
}
