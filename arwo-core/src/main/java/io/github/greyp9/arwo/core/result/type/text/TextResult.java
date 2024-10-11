package io.github.greyp9.arwo.core.result.type.text;

import io.github.greyp9.arwo.core.result.type.Result;

public class TextResult extends Result {
    private final String text;

    public final String getText() {
        return text;
    }

    public TextResult(final String id, final String type, final String text) {
        super(id, type);
        this.text = text;
    }
}
