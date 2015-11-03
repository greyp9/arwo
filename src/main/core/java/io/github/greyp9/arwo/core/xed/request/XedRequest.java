package io.github.greyp9.arwo.core.xed.request;

import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.state.XedUserState;

public class XedRequest {
    private final ServletHttpRequest request;
    private final XedSession session;
    private final XedUserState state;

    public final ServletHttpRequest getHttpRequest() {
        return request;
    }

    public final XedSession getSession() {
        return session;
    }

    public final XedUserState getState() {
        return state;
    }

    public XedRequest(final ServletHttpRequest request, final XedSession session, final XedUserState state) {
        this.request = request;
        this.session = session;
        this.state = state;
    }
}
