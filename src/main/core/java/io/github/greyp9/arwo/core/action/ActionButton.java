package io.github.greyp9.arwo.core.action;

public class ActionButton extends ActionItem {
    private final String label;

    public final String getLabel() {
        return label;
    }

    public ActionButton(final String label, final String name,
                        final String subject, final String action, final String object) {
        super(name, subject, action, object);
        this.label = label;
    }
}
