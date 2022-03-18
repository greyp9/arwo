package io.github.greyp9.arwo.core.text.filter;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.BinaryOperator;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.expr.op.UnaryOperator;
import io.github.greyp9.arwo.core.expr.token.Token;

import java.util.List;
import java.util.regex.Pattern;

public final class TextMatchTree {
    private final Node root;

    public Node getRoot() {
        return root;
    }

    public TextMatchTree(final Node root) {
        this.root = Transform.toNode(root);
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
        } else if (node instanceof MultiOperator) {
            evaluate = evaluate((MultiOperator) node, text);
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

    /**
     * The only function currently supported is "regex".
     */
    private static boolean evaluate(final MultiOperator operator, final String text) {
        boolean evaluate = false;
        final String op = operator.getOp();
        final List<Node> operands = operator.getOperands();
        final Node operand = operands.get(0);
        if (Token.Const.REGEX.equalsIgnoreCase(op)) {
            evaluate = evaluateRegex((RegexOperand) operand, text);
        }
        return evaluate;
    }

    private static boolean evaluateRegex(final RegexOperand operand, final String text) {
        final Pattern pattern = operand.getValue();
        return pattern.matcher(text).find();
    }
}
