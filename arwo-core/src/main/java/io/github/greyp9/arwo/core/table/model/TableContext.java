package io.github.greyp9.arwo.core.table.model;

import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;

import java.util.Date;

public class TableContext {
    private final ViewState viewState;
    private final XedActionFilter filter;
    private final String submitID;
    private final String tableClass;
    private final boolean refreshControl;
    private final Bundle bundle;
    private final Locus locus;
    private final Date date;

    public final ViewState getViewState() {
        return viewState;
    }

    public final XedActionFilter getFilter() {
        return filter;
    }

    public final String getSubmitID() {
        return submitID;
    }

    public final boolean getRefreshControl() {
        return refreshControl;
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

    public final Date getDate() {
        return date;
    }

    public TableContext(final ViewState viewState, final XedActionFilter filter, final String submitID,
                        final String tableClass, final Bundle bundle, final Locus locus) {
        this(viewState, filter, submitID, tableClass, false, bundle, locus, new Date());
    }

    public TableContext(final ViewState viewState, final XedActionFilter filter, final String submitID,
                        final String tableClass, final Bundle bundle, final Locus locus, final Date date) {
        this(viewState, filter, submitID, tableClass, false, bundle, locus, date);
    }

    @SuppressWarnings("checkstyle:parameternumber")
    public TableContext(final ViewState viewState, final XedActionFilter filter, final String submitID,
                        final String tableClass, final boolean refreshControl,
                        final Bundle bundle, final Locus locus, final Date date) {
        this.viewState = viewState;
        this.filter = filter;
        this.submitID = submitID;
        this.tableClass = tableClass;
        this.refreshControl = refreshControl;
        this.bundle = bundle;
        this.locus = locus;
        this.date = date;
    }
}
