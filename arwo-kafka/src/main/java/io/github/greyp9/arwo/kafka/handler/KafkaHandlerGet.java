package io.github.greyp9.arwo.kafka.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.kafka.client.ConsumeKafkaClient;
import io.github.greyp9.arwo.kafka.view.KafkaMessageView;
import io.github.greyp9.arwo.kafka.view.KafkaMessagesView;

import java.io.IOException;

public class KafkaHandlerGet {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final ConsumeKafkaClient consumeKafkaClient;

    public KafkaHandlerGet(final ServletHttpRequest httpRequest,
                           final AppUserState userState,
                           final ConsumeKafkaClient consumeKafkaClient) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.consumeKafkaClient = consumeKafkaClient;
    }

    public final HttpResponse doGet() throws IOException {
        final String baseURI = httpRequest.getBaseURI();
        final String pathInfo = httpRequest.getPathInfo();
        final Pather patherMessage = new Pather(httpRequest.getPathInfo());
        final String message = patherMessage.getLeftToken();
        final Pather patherContext = new Pather(patherMessage.getRight());
        final String context = patherContext.getLeftToken();

        HttpResponse httpResponse;
        if (pathInfo == null) {
            httpResponse = HttpResponseU.to302(PathU.toDir(baseURI));
        } else if (!Value.isEmpty(context)) {
            httpResponse = new KafkaMessageView(httpRequest, userState, consumeKafkaClient).render(message, context);
        } else if (!Value.isEmpty(message)) {
            httpResponse = new KafkaMessageView(httpRequest, userState, consumeKafkaClient).render(message, null);
        } else {
            httpResponse = new KafkaMessagesView(httpRequest, userState, consumeKafkaClient).toHttpResponse();
        }
        return httpResponse;
    }
}
