package io.github.greyp9.arwo.core.action;

import io.github.greyp9.arwo.core.bundle.Bundle;

import java.util.Collection;

public class ActionButtons {
    private final String submitID;
    private final Bundle bundle;
    private final boolean expander;
    private final Collection<ActionButton> buttons;

    public final String getSubmitID() {
        return submitID;
    }

    public final Bundle getBundle() {
        return bundle;
    }

    public final boolean isExpander() {
        return expander;
    }

    public final Collection<ActionButton> getButtons() {
        return buttons;
    }

    public ActionButtons(final String submitID, final Bundle bundle,
                         final boolean expander, final Collection<ActionButton> buttons) {
        this.submitID = submitID;
        this.bundle = bundle;
        this.expander = expander;
        this.buttons = buttons;
    }
}
