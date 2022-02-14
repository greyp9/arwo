package io.github.greyp9.arwo.core.expr;

public final class Operand extends Node {
    private final String value;

    public String getValue() {
        return value;
    }

    public Operand(final String value) {
        this.value = value;
    }

    @Override
    public String render() {
        return value;
    }
}
