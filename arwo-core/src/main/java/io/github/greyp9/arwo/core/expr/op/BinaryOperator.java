package io.github.greyp9.arwo.core.expr.op;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operator;

public final class BinaryOperator extends Operator {
    private final Node left;
    private final Node right;

    public Node getLeft() {
        return left;
    }

    public Node getRight() {
        return right;
    }

    public BinaryOperator(final String op, final Node left, final Node right) {
        super(op);
        this.left = left;
        this.right = right;
    }

    @Override
    public String render() {
        return String.format("(%s %s %s)", left.render(), getOp(), right.render());
    }
}
