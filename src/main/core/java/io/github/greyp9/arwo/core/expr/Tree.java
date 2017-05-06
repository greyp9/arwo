package io.github.greyp9.arwo.core.expr;

public class Tree {
    private final Node root;

    public Node getRoot() {
        return root;
    }

    public Tree(Node root) {
        this.root = root;
    }

    public boolean evaluate(final String value) {
        return evaluate(root, value);
    }

    private static boolean evaluate(Node node, String text) {
        boolean evaluate = false;
        if (node instanceof Operand) {
            evaluate = evaluate((Operand) node, text);
        } else if (node instanceof Operator) {
            evaluate = evaluate((Operator) node, text);
        }
        return evaluate;
    }

    private static boolean evaluate(Operator operator, final String text) {
        boolean evaluate = false;
        final Node left = operator.getLeft(), right = operator.getRight();
        final String op = operator.getValue();
        if (Token.Const.AND.equalsIgnoreCase(op)) {
            evaluate = evaluate(left, text) && evaluate(right, text);
        } else if (Token.Const.OR.equalsIgnoreCase(op)) {
            evaluate = evaluate(left, text) || evaluate(right, text);
        } else if (Token.Const.NOT.equalsIgnoreCase(op) && (null == left)) {
            evaluate = !evaluate(right, text);
        }
        return evaluate;
    }

    private static boolean evaluate(Operand operand, final String text) {
        return text.contains(operand.getValue());
    }
}
