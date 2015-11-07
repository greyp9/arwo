package io.github.greyp9.arwo.app.core.state;

import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.state.ViewStates;

public class AppUserState {
    private final String submitID;
    private final ViewStates viewStates;
    private final Locus locus;

    public final String getSubmitID() {
        return submitID;
    }

    public final ViewStates getViewStates() {
        return viewStates;
    }

    public final Locus getLocus() {
        return locus;
    }

    public AppUserState(final String submitID, final ViewStates viewStates, final Locus locus) {
        this.submitID = submitID;
        this.viewStates = viewStates;
        this.locus = locus;
    }
}
