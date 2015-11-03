package io.github.greyp9.arwo.core.xed.state;

import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.xed.session.XedSession;

public class XedUserState {
    private final String submitID;
    private final ViewStates viewStates;
    private final XedSession session;

    public final String getSubmitID() {
        return submitID;
    }

    public final ViewStates getViewStates() {
        return viewStates;
    }

    public final XedSession getSession() {
        return session;
    }

    public XedUserState(final String submitID, final ViewStates viewStates, final XedSession session) {
        this.submitID = submitID;
        this.viewStates = viewStates;
        this.session = session;
    }
}
