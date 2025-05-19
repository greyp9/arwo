package io.github.greyp9.arwo.slack.sample;

import com.slack.api.Slack;
import com.slack.api.methods.MethodsClient;
import com.slack.api.methods.SlackApiException;
import com.slack.api.methods.request.chat.ChatPostMessageRequest;
import com.slack.api.methods.response.chat.ChatPostMessageResponse;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

public class SlackWriterRunnable implements Runnable {
    private final Logger logger = LoggerFactory.getLogger(getClass());

    private final String botToken;
    private final String channelId;
    private final AtomicReference<String> reference;
    private final List<String> output;
    private final long interval;

    public SlackWriterRunnable(
            final AtomicReference<String> reference,
            final String botToken,
            final String channelId,
            final List<String> output,
            final long interval) {
        this.reference = reference;
        this.botToken = botToken;
        this.channelId = channelId;
        this.output = output;
        this.interval = interval;
    }

    @Override
    public final void run() {
        logger.info("BEGIN");
        final MethodsClient client = Slack.getInstance().methods(botToken);
        while (reference.get() == null) {
            processOutput(client);
            MutexU.wait(reference, interval);  // worker should process first
        }
        logger.info("END: {}", reference.get());
    }

    private void processOutput(final MethodsClient client) {
        while (!output.isEmpty()) {
            final String output1 = output.remove(0);
            final ChatPostMessageRequest request = ChatPostMessageRequest.builder()
                    .channel(channelId).text(output1).build();
            try {
                final ChatPostMessageResponse response = client.chatPostMessage(request);
                logger.info("ChatPostMessage: SENT={}", response.isOk());
            } catch (IOException | SlackApiException e) {
                logger.info(e.getMessage());
            }
        }
    }
}
