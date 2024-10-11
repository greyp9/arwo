package io.github.greyp9.arwo.core.action;

public class ActionButton extends ActionItem {
    private final String title;
    private final String label;

    public final String getTitle() {
        return title;
    }

    public final String getLabel() {
        return label;
    }

    public ActionButton(final String title, final String label, final String name,
                        final String subject, final String action, final String object, final String object2) {
        super(name, subject, action, object, object2);
        this.title = title;
        this.label = label;
    }
}
