package io.github.greyp9.arwo.app.core.action;

import io.github.greyp9.arwo.app.action.DeferredAction;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.action.AlertActions;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cache.ResourceCache;

import java.io.IOException;

public class ActionCacheClear extends DeferredAction {
    private final AlertActions actions;
    private final ResourceCache cache;

    public final AlertActions getActions() {
        return actions;
    }

    public ActionCacheClear(final ResourceCache cache) {
        super(Const.ID);
        this.actions = new AlertActions(Const.ID, Const.CANCEL, Const.CONFIRM);
        this.cache = cache;
    }

    @Override
    public final void doAction(final String option, final Bundle bundle, final Alerts alerts) {
        if (Const.CONFIRM.equals(option)) {
            try {
                final long size = cache.getSize();
                cache.clear();
                alerts.add(new Alert(Alert.Severity.INFO, bundle.format("AppUserState.cache.clear", size)));
            } catch (IOException e) {
                alerts.add(new Alert(Alert.Severity.ERR, e.getMessage(), e.getClass().getName(), null));
            }
        }
    }

    public static class Const {
        private static final String ID = "cache.clear";
        private static final String CANCEL = "cache.clear.cancel";
        private static final String CONFIRM = "cache.clear.confirm";
    }
}
