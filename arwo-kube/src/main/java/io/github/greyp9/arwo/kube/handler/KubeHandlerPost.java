package io.github.greyp9.arwo.kube.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppText;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.submit.SubmitTokenU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;

import java.io.IOException;

public class KubeHandlerPost {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public KubeHandlerPost(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doPost() {
        HttpResponse httpResponse;
        try {
            httpResponse = doPostInternal();
        } catch (IOException e) {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
            httpResponse = HttpResponseU.to500(e.getMessage());
        }
        return httpResponse;
    }

    private HttpResponse doPostInternal() throws IOException {
        final byte[] entity = StreamU.read(httpRequest.getHttpRequest().getEntity());
        final NameTypeValues nameTypeValues = HttpArguments.toArguments(entity);
        final String submitID = userState.getSubmitID();
        HttpResponse httpResponse = HttpResponseU.to302(httpRequest.getURI());
        for (final NameTypeValue nameTypeValue : nameTypeValues) {
            if (submitID.equals(nameTypeValue.getName())) {
                final SubmitToken token = SubmitTokenU.fromString(nameTypeValue.getValueS());
                if (token != null) {
                    final String subject = token.getSubject();
                    if (App.Target.VIEW_STATE.equals(subject)) {
                        final Bundle bundle = new Bundle(new AppText(userState.getLocus().getLocale()).getBundleCore());
                        userState.getViewStates().apply(token, nameTypeValues, bundle, new Alerts());
                    }
                }
            }
        }
        return httpResponse;
    }
}
