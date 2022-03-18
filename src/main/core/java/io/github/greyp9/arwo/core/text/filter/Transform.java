package io.github.greyp9.arwo.core.text.filter;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.BinaryOperator;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.expr.op.UnaryOperator;
import io.github.greyp9.arwo.core.expr.token.Token;

import java.util.List;

/**
 * Augment the parameter {@link Node} tree with custom {@link Operand} objects to implement enhanced behaviors.
 */
public final class Transform {

    private Transform() {
    }

    public static Node toNode(final Node node) {
        final Node nodeA;
        if (node instanceof Operand) {
            final Operand operand = (Operand) node;
            nodeA = new Operand(operand.getValue());
        } else if (node instanceof BinaryOperator) {
            final BinaryOperator operator = (BinaryOperator) node;
            nodeA = new BinaryOperator(operator.getOp(), toNode(operator.getLeft()), toNode(operator.getRight()));
        } else if (node instanceof UnaryOperator) {
            final UnaryOperator operator = (UnaryOperator) node;
            nodeA = new UnaryOperator(operator.getOp(), toNode(operator.getOperand()));
        } else if (node instanceof MultiOperator) {
            nodeA = toNode((MultiOperator) node);
        } else {
            throw new IllegalStateException(Node.class.getName());
        }
        return nodeA;
    }

    private static Node toNode(final MultiOperator operator) {
        final Node nodeA;
        final String op = operator.getOp();
        final List<Node> operands = operator.getOperands();
        final Node node = operands.get(0);
        if (Token.Const.REGEX.equalsIgnoreCase(op)) {
            final Operand operand = (Operand) node;
            nodeA = new MultiOperator(op, new RegexOperand(operand.getValue()));
        } else {
            nodeA = new MultiOperator(op, operands);
        }
        return nodeA;
    }
}
