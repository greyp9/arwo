package io.github.greyp9.arwo.core.result.view;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.link.MetaLink;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.state.ViewStates;

public class ResultsContext {
    private final ViewStates viewStates;
    private final Locus locus;
    private final Bundle bundle;
    private final Alerts alerts;
    private final String submitID;
    private final MetaLink metaLink;

    public final ViewStates getViewStates() {
        return viewStates;
    }

    public final Locus getLocus() {
        return locus;
    }

    public final Bundle getBundle() {
        return bundle;
    }

    public final Alerts getAlerts() {
        return alerts;
    }

    public final String getSubmitID() {
        return submitID;
    }

    public final MetaLink getMetaLink() {
        return metaLink;
    }

    public ResultsContext(final ViewStates viewStates, final Locus locus, final Bundle bundle,
                          final Alerts alerts, final String submitID, final MetaLink metaLink) {
        this.viewStates = viewStates;
        this.locus = locus;
        this.bundle = bundle;
        this.alerts = alerts;
        this.submitID = submitID;
        this.metaLink = metaLink;
    }

    public static ResultsContext createEmpty() {
        return new ResultsContext(null, null, null, null, null, new MetaLink(null, null));
    }
}
