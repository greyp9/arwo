package io.github.greyp9.arwo.core.expr;

public class Operand extends Node {
    private final String value;

    public String getValue() {
        return value;
    }

    public Operand(String value) {
        super(null, null);
        this.value = value;
    }

    @Override
    public String render() {
        return value;
    }
}
