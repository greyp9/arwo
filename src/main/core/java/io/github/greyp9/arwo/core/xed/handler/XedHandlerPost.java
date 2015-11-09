package io.github.greyp9.arwo.core.xed.handler;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.submit.SubmitTokenU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.write.XedWrite;

import java.io.IOException;

public class XedHandlerPost {
    private final XedRequest request;

    public XedHandlerPost(final XedRequest request) {
        this.request = request;
    }

    public final HttpResponse doPost() throws IOException {
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final byte[] entity = StreamU.read(httpRequest.getHttpRequest().getEntity());
        final NameTypeValues nameTypeValues = HttpArguments.toArguments(entity);
        final String submitID = request.getState().getSubmitID();
        HttpResponse httpResponse = HttpResponseU.toHttpResponse302(httpRequest.getURI());
        for (final NameTypeValue nameTypeValue : nameTypeValues) {
            if (submitID.equals(nameTypeValue.getName())) {
                final SubmitToken token = SubmitTokenU.fromString(nameTypeValue.getValueS());
                if (token != null) {
                    httpResponse = apply(token, nameTypeValues, httpResponse);
                }
            }
        }
        return httpResponse;
    }

    private HttpResponse apply(final SubmitToken token, final NameTypeValues nameTypeValues,
                               final HttpResponse httpResponseIn) throws IOException {
        HttpResponse httpResponse = httpResponseIn;
        final String subject = token.getSubject();
        if (App.Target.DOCUMENT.equals(subject)) {
            httpResponse = new XedWrite(request).apply(token, nameTypeValues, httpResponse);
        } else if (App.Target.USER_STATE.equals(subject)) {
            request.getState().apply(token, nameTypeValues);
        } else if (App.Target.VIEW_STATE.equals(subject)) {
            request.getState().getViewStates().apply(token, nameTypeValues);
        }
        return httpResponse;
    }
}
