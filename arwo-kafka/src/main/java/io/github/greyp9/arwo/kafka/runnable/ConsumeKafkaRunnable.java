package io.github.greyp9.arwo.kafka.runnable;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;
import io.github.greyp9.arwo.kafka.client.ConsumeKafkaClient;
import org.apache.kafka.clients.consumer.Consumer;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

public class ConsumeKafkaRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final ConsumeKafkaClient client;

    private static final int MAX_RECORDS = 100;

    public ConsumeKafkaRunnable(final ConsumeKafkaClient client) {
        super();
        this.client = client;
    }

    @Override
    public final void run() {
        logger.entering(getClass().getName(), Runnable.class.getName());
        final AtomicReference<String> reference = client.getReference();
        try (KafkaConsumer<byte[], byte[]> consumer = client.getConsumer()) {
            consumer.subscribe(client.getTopics());
            while (reference.get() == null) {
                if (client.getRecords().size() < MAX_RECORDS) {
                    client.addRecords(queryRecords(consumer, 1, Duration.ofSeconds(1)));
                }
                MutexU.wait(reference, DurationU.Const.ONE_SECOND_MILLIS);
            }
        }
        logger.exiting(getClass().getName(), Runnable.class.getName());
    }

    private <T> Collection<ConsumerRecord<T, T>> queryRecords(
            final Consumer<T, T> consumer, final int iterations, final Duration duration) {
        final Collection<ConsumerRecord<T, T>> records = new ArrayList<>();
        for (int i = 0; (i < iterations); ++i) {
            final ConsumerRecords<T, T> recordsIt = consumer.poll(duration);
            for (final ConsumerRecord<T, T> record : recordsIt) {
                records.add(record);
            }
        }
        return records;
    }
}
