package io.github.greyp9.arwo.kafka.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.kafka.client.ConsumeKafkaClient;
import io.github.greyp9.arwo.kafka.core.ArwoKafka;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.w3c.dom.Document;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Optional;
import java.util.logging.Logger;

public class KafkaMessageView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final ConsumeKafkaClient consumeKafkaClient;

    public KafkaMessageView(final ServletHttpRequest httpRequest,
                            final AppUserState userState,
                            final ConsumeKafkaClient consumeKafkaClient) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.consumeKafkaClient = consumeKafkaClient;
    }

    public final HttpResponse render(final String message, final String context) throws IOException {
        Logger.getLogger(getClass().getName()).finest(userState.getSubmitID());
        final Optional<ConsumerRecord<byte[], byte[]>> record = consumeKafkaClient.getRecords().stream()
                .filter(r -> Long.toString(r.offset()).equals(message))
                .findFirst();
        final HttpResponse httpResponse;
        if (record.isPresent()) {
            httpResponse = render(record.get(), message, context);
        } else {
            httpResponse = HttpResponseU.to404();
        }
        return httpResponse;
    }

    private HttpResponse render(final ConsumerRecord<byte[], byte[]> record,
                                final String message, final String context) throws IOException {
        final HttpResponse httpResponse;
        if (ArwoKafka.KEY.equals(context)) {
            httpResponse = HttpResponseU.to200(toMetaFile(record.key(), Http.Mime.TEXT_PLAIN_UTF8));
        } else if (ArwoKafka.VALUE.equals(context)) {
            httpResponse = HttpResponseU.to200(toMetaFile(record.value(), Http.Mime.TEXT_PLAIN_UTF8));
        } else if (context != null) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getBaseURI(), message));
        } else {
            final Document document = DocumentU.createDocument(record.getClass().getSimpleName(), ArwoKafka.NAMESPACE);
            ElementU.addElement(document.getDocumentElement(), ArwoKafka.KEY, UTF8Codec.toString(record.key()));
            ElementU.addElement(document.getDocumentElement(), ArwoKafka.VALUE, UTF8Codec.toString(record.value()));
            httpResponse = HttpResponseU.to200(toMetaFile(DocumentU.toXml(document), Http.Mime.TEXT_XML_UTF8));
        }
        return httpResponse;
    }

    private MetaFile toMetaFile(final byte[] payload, final String type) {
        final FileMetaData metaData = new FileMetaData(null, payload.length, 0L, false);
        return new MetaFile(metaData, type, new ByteArrayInputStream(payload));
    }
}
