package io.github.greyp9.arwo.kafka;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.clients.producer.RecordMetadata;
import org.apache.kafka.common.serialization.ByteArraySerializer;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Date;
import java.util.Properties;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class KafkaSaslTest {
    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final String PATH_KAFKA_IMAGES = System.getProperty("user.home") + "/kafka-images";
    private static final String PATH_SASL = PATH_KAFKA_IMAGES + "/examples/kafka-cluster-sasl";
    private static final String KAFKA_TIMEOUT = "15000";

    @Test
    @Disabled("requires external setup")
    void testSendOne() throws ExecutionException, InterruptedException {
        logger.info("test::BEGIN");
        System.setProperty("java.security.krb5.conf", PATH_SASL + "/secrets/krb.conf");
        //System.setProperty("sun.security.krb5.debug", "true");
        //System.setProperty("java.security.auth.login.config", "/secrets/producer_jaas.conf");

        final Properties properties = getProperties();
        try (ByteArraySerializer serializer = new ByteArraySerializer()) {
            final KafkaProducer<byte[], byte[]> kafkaProducer = new KafkaProducer<>(properties, serializer, serializer);
            //kafkaProducer.initTransactions();
            //kafkaProducer.beginTransaction();
            for (int i = 0; (i < 2); ++i) {
                final ProducerRecord<byte[], byte[]> record = new ProducerRecord<>(
                        "test1", "key".getBytes(), XsdDateU.toXSDZMillis(new Date()).getBytes());
                final Future<RecordMetadata> metadata = kafkaProducer.send(record);
                ThreadU.sleepMillis(DurationU.Const.ONE_SECOND_MILLIS);
                final RecordMetadata recordMetadata = metadata.get();
                logger.info("recordMetadata={}", recordMetadata);
                kafkaProducer.flush();
            }
            //kafkaProducer.commitTransaction();
        }
        logger.info("test::END");
    }

    private static final String SSL_STORE_TYPE = "JKS";
    private static final String SSL_PASSWORD = "confluent";
    private static final String SSL_PROTOCOL = "TLSv1.2";

    private static Properties getProperties() {
        final Properties properties = new Properties();
        properties.setProperty("bootstrap.servers",
                "kafka-sasl-1.eden:19094,kafka-sasl-2.eden:29094,kafka-sasl-3.eden:39094");
        properties.setProperty("security.protocol", "SASL_SSL");
        properties.setProperty("sasl.mechanism", "GSSAPI");
        properties.setProperty("sasl.kerberos.service.name", "kafka");
        properties.setProperty("sasl.jaas.config",
                "com.sun.security.auth.module.Krb5LoginModule required useKeyTab=true "
                + "keyTab=\"" + PATH_SASL + "/secrets/sasl/producer2.keytab\" "
                + "principal=\"producer2/kafka-sasl-en0@TEST.CONFLUENT.IO\";");

        properties.setProperty("ssl.keystore.location", PATH_SASL + "/secrets/ssl/kafka.producer.keystore.jks");
        properties.setProperty("ssl.keystore.type", SSL_STORE_TYPE);
        properties.setProperty("ssl.keystore.password", SSL_PASSWORD);
        properties.setProperty("ssl.key.password", SSL_PASSWORD);
        properties.setProperty("ssl.truststore.location", PATH_SASL + "/secrets/ssl/kafka.producer.truststore.jks");
        properties.setProperty("ssl.truststore.type", SSL_STORE_TYPE);
        properties.setProperty("ssl.truststore.password", SSL_PASSWORD);
        properties.setProperty("ssl.enabled.protocols", SSL_PROTOCOL);
        properties.setProperty("ssl.protocol", SSL_PROTOCOL);

        //final String uuid = UUID.randomUUID().toString();
        //properties.setProperty("transactional.id", uuid);
        properties.setProperty("request.timeout.ms", KAFKA_TIMEOUT);
        properties.setProperty("delivery.timeout.ms", KAFKA_TIMEOUT);
        properties.setProperty("transaction.timeout.ms", KAFKA_TIMEOUT);
        return properties;
    }
}
