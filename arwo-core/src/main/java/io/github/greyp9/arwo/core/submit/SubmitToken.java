package io.github.greyp9.arwo.core.submit;

import io.github.greyp9.arwo.core.value.Value;

public class SubmitToken {
    private final String subject;
    private final String action;
    private final String object;
    private final String object2;

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

    public SubmitToken(final String subject, final String action) {
        this(subject, action, "", "");
    }

    public SubmitToken(final String subject, final String action, final String object) {
        this(subject, action, object, "");
    }

    public SubmitToken(final String subject, final String action, final String object, final String object2) {
        this.subject = subject;
        this.action = action;
        this.object = object;
        this.object2 = object2;
    }

    // encoder
    public final String toString() {
        return String.format("[%s][%s][%s][%s]", subject, action, object, Value.defaultOnNull(object2, ""));
    }
}
