package io.github.greyp9.arwo.core.expr.op;

import io.github.greyp9.arwo.core.expr.Node;

public final class UnaryOperator extends Node {
    private final String op;
    private final Node operand;

    public String getOp() {
        return op;
    }

    public Node getOperand() {
        return operand;
    }

    public UnaryOperator(final String op, final Node operand) {
        this.op = op;
        this.operand = operand;
    }

    @Override
    public String render() {
        return String.format("(%s %s)", op, operand.render());
    }
}
