package io.github.greyp9.arwo.core.expr;

public final class Operator extends Node {
    private final String value;

    public String getValue() {
        return value;
    }

    public Operator(final Node left, final Node right, final String value) {
        super(left, right);
        this.value = value;
    }

    @Override
    public String render() {
        return String.format("(%s %s %s)", getLeft().render(), value, getRight().render());
    }
}
