package io.github.greyp9.arwo.core.result.type.text;

import io.github.greyp9.arwo.core.result.type.Result;

public class TextResult extends Result {
    private final String text;

    public String getText() {
        return text;
    }

    public TextResult(String id, String type, String text) {
        super(id, type);
        this.text = text;
    }
}
