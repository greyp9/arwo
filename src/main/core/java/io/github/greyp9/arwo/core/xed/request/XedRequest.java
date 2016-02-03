package io.github.greyp9.arwo.core.xed.request;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.AppText;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.state.XedUserState;

import java.util.Locale;

public class XedRequest {
    private final ServletHttpRequest request;
    private final XedSession session;
    private final XedUserState state;
    private final Bundle bundle;
    private final Alerts alerts;

    public final ServletHttpRequest getHttpRequest() {
        return request;
    }

    public final XedSession getSession() {
        return session;
    }

    public final XedUserState getState() {
        return state;
    }

    public final Bundle getBundle() {
        return bundle;
    }

    public final Alerts getAlerts() {
        return alerts;
    }

    public XedRequest(final ServletHttpRequest request, final XedSession session, final XedUserState state) {
        this.request = request;
        this.session = session;
        this.state = state;
        this.bundle = new Bundle(new AppText(state.getLocus().getLocale()).getBundleCore());
        this.alerts = state.getAlerts();
    }

    public final XedFactory getFactory() {
        return state.getFactory();
    }

    public final Locale getLocale() {
        return state.getLocus().getLocale();
    }

    public final char[] getSecret() {
        return Value.toCharArray(request.getHttpRequest().getHeader(Http.Header.AUTHORIZATION));
    }
}
