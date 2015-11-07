package io.github.greyp9.arwo.app.core.state;

import io.github.greyp9.arwo.core.table.state.ViewStates;

public class AppUserState {
    private final String submitID;
    private final ViewStates viewStates;

    public final String getSubmitID() {
        return submitID;
    }

    public final ViewStates getViewStates() {
        return viewStates;
    }

    public AppUserState(final String submitID, final ViewStates viewStates) {
        this.submitID = submitID;
        this.viewStates = viewStates;
    }
}
