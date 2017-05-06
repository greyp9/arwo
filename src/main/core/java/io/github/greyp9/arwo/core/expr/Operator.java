package io.github.greyp9.arwo.core.expr;

public class Operator extends Node {
    private final String value;

    public String getValue() {
        return value;
    }

    public Operator(Node left, Node right, String value) {
        super(left, right);
        this.value = value;
    }

    @Override
    public String render() {
        return String.format("(%s %s %s)", getLeft().render(), value, getRight().render());
    }
}
