package io.github.greyp9.arwo.core.xed.handler;

import io.github.greyp9.arwo.core.alert.Alert;
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
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.state.XedUserState;
import io.github.greyp9.arwo.core.xed.write.XedWrite;

import java.io.IOException;

public class XedHandlerPost {
    private final ServletHttpRequest httpRequest;
    private final XedUserState documentState;

    public XedHandlerPost(final ServletHttpRequest httpRequest, final XedUserState documentState) {
        this.httpRequest = httpRequest;
        this.documentState = documentState;
    }

    public final HttpResponse doPostSafe() throws IOException {
        HttpResponse httpResponse;
        try {
            httpResponse = doPost();
        } catch (IOException e) {
            documentState.getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
            httpResponse = HttpResponseU.to500(e.getMessage());
        }
        return httpResponse;
    }

    private HttpResponse doPost() throws IOException {
        final byte[] entity = StreamU.read(httpRequest.getHttpRequest().getEntity());
        final NameTypeValues httpArguments = HttpArguments.toArguments(entity);
        final String submitID = documentState.getSubmitID();
        String location = httpRequest.getURI();
        for (final NameTypeValue httpArgument : httpArguments) {
            if (submitID.equals(httpArgument.getName())) {
                final SubmitToken token = SubmitTokenU.fromString(httpArgument.getValueS());
                if (token != null) {
                    location = apply(token, httpArguments, location);
                }
            }
        }
        return HttpResponseU.to302(location);
    }

    private String apply(final SubmitToken token, final NameTypeValues httpArguments,
                         final String locationIn) throws IOException {
        final XedSession session = documentState.getSession(httpRequest.getServletPath());
        final XedRequest request = new XedRequest(httpRequest, session, documentState);
        String location = locationIn;
        final String subject = token.getSubject();
        if (App.Target.DOCUMENT.equals(subject)) {
            location = new XedWrite(request).apply(token, httpArguments);
        } else if (App.Target.SESSION.equals(subject)) {
            documentState.applySession(token, httpArguments, request);
        } else if (App.Target.USER_STATE.equals(subject)) {
            location = documentState.applyPost(token, httpArguments, request);
        } else if (App.Target.VIEW_STATE.equals(subject)) {
            documentState.getViewStates().apply(token, httpArguments, request.getBundle(), request.getAlerts());
        }
        documentState.applyLocale();
        return location;
    }
}
