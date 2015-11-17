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
import io.github.greyp9.arwo.core.xed.state.XedUserState;
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
        final NameTypeValues httpArguments = HttpArguments.toArguments(entity);
        final String submitID = request.getState().getSubmitID();
        String location = httpRequest.getURI();
        for (final NameTypeValue httpArgument : httpArguments) {
            if (submitID.equals(httpArgument.getName())) {
                final SubmitToken token = SubmitTokenU.fromString(httpArgument.getValueS());
                if (token != null) {
                    location = apply(token, httpArguments, location);
                }
            }
        }
        return HttpResponseU.toHttpResponse302(location);
    }

    private String apply(final SubmitToken token, final NameTypeValues httpArguments,
                         final String locationIn) throws IOException {
        String location = locationIn;
        final XedUserState userState = request.getState();
        final String subject = token.getSubject();
        if (App.Target.DOCUMENT.equals(subject)) {
            location = new XedWrite(request).apply(token, httpArguments);
        } else if (App.Target.SESSION.equals(subject)) {
            userState.applySession(token, httpArguments, request);
        } else if (App.Target.USER_STATE.equals(subject)) {
            location = userState.applyPost(token, httpArguments, request);
        } else if (App.Target.VIEW_STATE.equals(subject)) {
            userState.getViewStates().apply(token, httpArguments, request.getBundle(), request.getAlerts());
        }
        return location;
    }
}
