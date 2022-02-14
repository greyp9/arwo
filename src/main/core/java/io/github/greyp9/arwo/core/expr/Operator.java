package io.github.greyp9.arwo.core.expr;

public abstract class Operator extends Node {
    private final String op;

    public final String getOp() {
        return op;
    }

    public Operator(final String op) {
        this.op = op;
    }
}
