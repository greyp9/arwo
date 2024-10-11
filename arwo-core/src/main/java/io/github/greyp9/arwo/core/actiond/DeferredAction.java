package io.github.greyp9.arwo.core.actiond;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;

@SuppressWarnings("PMD.AbstractNaming")
public abstract class DeferredAction {
    private final String id;

    public final String getID() {
        return id;
    }

    public DeferredAction(final String id) {
        this.id = id;
    }

    public abstract void doAction(String option, Bundle bundle, Alerts alerts);
}
