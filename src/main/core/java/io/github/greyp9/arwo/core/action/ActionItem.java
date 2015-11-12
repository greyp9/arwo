package io.github.greyp9.arwo.core.action;

public class ActionItem {
    private final String name;
    private final String subject;
    private final String action;
    private final String object;
    private final String object2;

    public final String getName() {
        return name;
    }

    public final String getSubject() {
        return subject;
    }

    public final String getAction() {
        return action;
    }

    public final String getObject() {
        return object;
    }

    public final String getObject2() {
        return object2;
    }

    public ActionItem(final String name, final String subject, final String action,
                      final String object, final String object2) {
        this.name = name;
        this.subject = subject;
        this.action = action;
        this.object = object;
        this.object2 = object2;
    }
}
