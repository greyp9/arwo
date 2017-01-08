package io.github.greyp9.arwo.app.core.action;

import io.github.greyp9.arwo.core.actiond.DeferredAction;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.action.AlertActions;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.value.Value;

public class ActionStop extends DeferredAction {
    private final AlertActions actions;

    public final AlertActions getActions() {
        return actions;
    }

    public ActionStop() {
        super(ActionStop.Const.ID);
        this.actions = new AlertActions(Const.ID, Const.CANCEL, Const.CONFIRM);
    }

    @Override
    public final void doAction(final String option, final Bundle bundle, final Alerts alerts) {
        if (Const.CONFIRM.equals(option)) {
            System.setProperty(App.Application.NAME, Value.join(
                    Http.Token.DOT, App.Application.QUIT, getClass().getName()));
        }
    }

    public static class Const {
        private static final String ID = "app.stop";
        private static final String CANCEL = "app.stop.cancel";
        private static final String CONFIRM = "app.stop.confirm";
    }
}
