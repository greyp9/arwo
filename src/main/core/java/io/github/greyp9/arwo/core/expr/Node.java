package io.github.greyp9.arwo.core.expr;

public abstract class Node {
    private final Node left;
    private final Node right;

    public final Node getLeft() {
        return left;
    }

    public final Node getRight() {
        return right;
    }

    public Node(final Node left, final Node right) {
        this.left = left;
        this.right = right;
    }

    public abstract String render();
}
