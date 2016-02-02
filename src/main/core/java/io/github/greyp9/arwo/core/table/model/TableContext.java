package io.github.greyp9.arwo.core.table.model;

import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;

public class TableContext {
    private final ViewState viewState;
    private final XedActionFilter filter;
    private final String submitID;
    private final String tableClass;
    private final Bundle bundle;
    private final Locus locus;

    public final ViewState getViewState() {
        return viewState;
    }

    public final XedActionFilter getFilter() {
        return filter;
    }

    public final String getSubmitID() {
        return submitID;
    }

    public final String getTableClass() {
        return tableClass;
    }

    public final Bundle getBundle() {
        return bundle;
    }

    public final Locus getLocus() {
        return locus;
    }

    public TableContext(final ViewState viewState, final XedActionFilter filter, final String submitID,
                        final String tableClass, final Bundle bundle, final Locus locus) {
        this.viewState = viewState;
        this.filter = filter;
        this.submitID = submitID;
        this.tableClass = tableClass;
        this.bundle = bundle;
        this.locus = locus;
    }
}
