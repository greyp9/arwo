package io.github.greyp9.arwo.core.text.filter;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.BinaryOperator;
import io.github.greyp9.arwo.core.expr.op.UnaryOperator;
import io.github.greyp9.arwo.core.expr.token.Token;

public final class TextMatchTree {
    private final Node root;

    public Node getRoot() {
        return root;
    }

    public TextMatchTree(final Node root) {
        this.root = root;
    }

    public boolean evaluate(final String value) {
        return evaluate(root, value);
    }

    private static boolean evaluate(final Node node, final String text) {
        boolean evaluate = false;
        if (node instanceof Operand) {
            evaluate = evaluate((Operand) node, text);
        } else if (node instanceof BinaryOperator) {
            evaluate = evaluate((BinaryOperator) node, text);
        } else if (node instanceof UnaryOperator) {
            evaluate = evaluate((UnaryOperator) node, text);
        }
        return evaluate;
    }

    private static boolean evaluate(final BinaryOperator operator, final String text) {
        boolean evaluate = false;
        final Node left = operator.getLeft();
        final Node right = operator.getRight();
        final String op = operator.getOp();
        if (Token.Const.AND.equalsIgnoreCase(op)) {
            evaluate = evaluate(left, text) && evaluate(right, text);
        } else if (Token.Const.OR.equalsIgnoreCase(op)) {
            evaluate = evaluate(left, text) || evaluate(right, text);
        }
        return evaluate;
    }

    private static boolean evaluate(final UnaryOperator operator, final String text) {
        boolean evaluate = false;
        final Node operand = operator.getOperand();
        final String op = operator.getOp();
        if (Token.Const.NOT.equalsIgnoreCase(op)) {
            evaluate = !evaluate(operand, text);
        }
        return evaluate;
    }

    private static boolean evaluate(final Operand operand, final String text) {
        return text.contains(operand.getValue());
    }
}
