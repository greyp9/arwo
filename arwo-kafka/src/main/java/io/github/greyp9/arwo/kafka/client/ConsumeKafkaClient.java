package io.github.greyp9.arwo.kafka.client;

import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.submit.SubmitTokenU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.vm.mutex.CollectionU;
import io.github.greyp9.arwo.kafka.runnable.ConsumeKafkaRunnable;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.common.serialization.ByteArrayDeserializer;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

/**
 * <a href="https://kafka.apache.org/documentation.html#gettingStarted">Kafka - Getting Started</a>
 */
public class ConsumeKafkaClient {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final ExecutorService executorService;
    private final Properties properties;
    private final ConsumeKafkaRunnable consumeKafkaRunnable;
    private final AtomicReference<String> reference;
    private final List<ConsumerRecord<byte[], byte[]>> consumerRecords;

    public ConsumeKafkaClient(final ExecutorService executorService, final Properties properties) {
        this.executorService = executorService;
        this.properties = properties;
        this.consumeKafkaRunnable = new ConsumeKafkaRunnable(this);
        this.reference = new AtomicReference<>("initialized");
        this.consumerRecords = new ArrayList<>();
    }

    public final AtomicReference<String> getReference() {
        return reference;
    }

    public final Collection<String> getTopics() {
        return Arrays.asList(properties.getProperty("topics").split(","));
    }

    public final void start() {
        reference.set(null);
        executorService.execute(consumeKafkaRunnable);
    }

    public final void stop(final String message) {
        reference.set(message);
    }

    public final KafkaConsumer<byte[], byte[]> getConsumer() {
        final ByteArrayDeserializer deserializer = new ByteArrayDeserializer();
        return new KafkaConsumer<>(properties, deserializer, deserializer);
    }

    public final Collection<ConsumerRecord<byte[], byte[]>> getRecords() {
        return CollectionU.copy(new ArrayList<>(), consumerRecords);
    }

    public final void addRecords(final Collection<ConsumerRecord<byte[], byte[]>> records) {
        CollectionU.copy(consumerRecords, records);
    }

    public final void update(final ServletHttpRequest httpRequest, final String submitID) throws IOException {
        final byte[] entity = StreamU.read(httpRequest.getHttpRequest().getEntity());
        final NameTypeValues nameTypeValues = HttpArguments.toArguments(entity);
        for (final NameTypeValue nameTypeValue : nameTypeValues) {
            if (submitID.equals(nameTypeValue.getName())) {
                final SubmitToken token = SubmitTokenU.fromString(nameTypeValue.getValueS());
                update(token);
            }
        }
    }

    private void update(final SubmitToken token) {
        final String action = token.getAction();
        logger.info(action);
        if (action.equals("start")) {
            start();
        } else if (action.equals("stop")) {
            stop("UI-STOP");
        } else if (action.equals("clear")) {
            consumerRecords.clear();
        }
    }
}
