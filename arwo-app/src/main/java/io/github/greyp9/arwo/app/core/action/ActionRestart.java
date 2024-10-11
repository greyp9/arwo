package io.github.greyp9.arwo.app.core.action;

import io.github.greyp9.arwo.core.actiond.DeferredAction;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.action.AlertActions;
import io.github.greyp9.arwo.core.app.AppSignal;
import io.github.greyp9.arwo.core.bundle.Bundle;

public class ActionRestart extends DeferredAction {
    private final AlertActions actions;

    public final AlertActions getActions() {
        return actions;
    }

    public ActionRestart() {
        super(ActionRestart.Const.ID);
        this.actions = new AlertActions(Const.ID, Const.CANCEL, Const.CONFIRM);
    }

    @Override
    public final void doAction(final String option, final Bundle bundle, final Alerts alerts) {
        if (Const.CONFIRM.equals(option)) {
            System.setProperty(AppSignal.NAME, getClass().getName());
        }
    }

    public static class Const {
        private static final String ID = "app.restart";
        private static final String CANCEL = "app.restart.cancel";
        private static final String CONFIRM = "app.restart.confirm";
    }
}
