package io.github.greyp9.arwo.slack.sample;

import com.slack.api.Slack;
import com.slack.api.methods.MethodsClient;
import com.slack.api.methods.SlackApiException;
import com.slack.api.methods.request.conversations.ConversationsHistoryRequest;
import com.slack.api.methods.response.conversations.ConversationsHistoryResponse;
import com.slack.api.model.Message;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

public class SlackReaderRunnable implements Runnable {
    private final Logger logger = LoggerFactory.getLogger(getClass());

    private final AtomicReference<String> reference;
    private final String botToken;
    private final String channelId;
    private final long interval;

    public SlackReaderRunnable(
            final AtomicReference<String> reference,
            final String botToken,
            final String channelId,
            final String interval) {
        this.reference = reference;
        this.botToken = botToken;
        this.channelId = channelId;
        this.interval = DurationU.toMillisP(interval);
    }

    @Override
    public final void run() {
        logger.info("BEGIN");
        final MethodsClient client = Slack.getInstance().methods(botToken);
        final AtomicReference<String> oldest = new AtomicReference<>();
        while (reference.get() == null) {
            processInput(client, oldest);
            MutexU.wait(reference, interval);  // worker should process first
        }
        logger.info("END: {}", reference.get());
    }

    private void processInput(final MethodsClient client, final AtomicReference<String> oldest) {
        // https://api.slack.com/methods/conversations.history#response
        logger.info("POLL: {}", oldest.get());
        final ConversationsHistoryRequest request = ConversationsHistoryRequest.builder()
                .channel(channelId).oldest(oldest.get()).inclusive(false).limit(1).build();
        try {
            final ConversationsHistoryResponse response = client.conversationsHistory(request);
            if (response.isOk()) {
                final List<Message> messages = Value.defaultOnNull(response.getMessages(), Collections.emptyList());
                for (Message message : messages) {
                    oldest.set(message.getTs());
                    final long secondsEpoch = Double.valueOf(message.getTs()).longValue();
                    final Date date = new Date(secondsEpoch * DurationU.Const.ONE_SECOND_MILLIS);
                    logger.info("MESSAGE:[{}][{}]", XsdDateU.toXSDZMillis(date), message.getText());
                }
            }
        } catch (IOException | SlackApiException e) {
            logger.info(e.getMessage());
        }
    }
}
