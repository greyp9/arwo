package io.github.greyp9.arwo.core.result.view;

import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.state.ViewStates;

public class ResultsContext {
    private final ViewStates viewStates;
    private final Locus locus;
    private final Bundle bundle;
    private final String submitID;

    public ViewStates getViewStates() {
        return viewStates;
    }

    public Locus getLocus() {
        return locus;
    }

    public Bundle getBundle() {
        return bundle;
    }

    public String getSubmitID() {
        return submitID;
    }


    public ResultsContext(ViewStates viewStates, Locus locus, Bundle bundle, String submitID) {
        this.viewStates = viewStates;
        this.locus = locus;
        this.bundle = bundle;
        this.submitID = submitID;
    }
}
