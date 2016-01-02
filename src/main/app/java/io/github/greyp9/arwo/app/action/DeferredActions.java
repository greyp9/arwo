package io.github.greyp9.arwo.app.action;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

public class DeferredActions {
    private final Collection<DeferredAction> actions;

    public DeferredActions() {
        this.actions = new ArrayList<DeferredAction>();
    }

    public final void add(final DeferredAction action) {
        actions.add(action);
    }

    public final void apply(final String instruction, final String id, final Bundle bundle, final Alerts alerts) {
        final Iterator<DeferredAction> iterator = actions.iterator();
        while (iterator.hasNext()) {
            final DeferredAction deferredAction = iterator.next();
            if (deferredAction.getID().equals(id)) {
                iterator.remove();
                deferredAction.doAction(instruction, bundle, alerts);
            }
        }
    }
}
