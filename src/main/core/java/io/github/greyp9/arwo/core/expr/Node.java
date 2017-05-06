package io.github.greyp9.arwo.core.expr;

public abstract class Node {
    private final Node left;
    private final Node right;

    public Node getLeft() {
        return left;
    }

    public Node getRight() {
        return right;
    }

    public Node(Node left, Node right) {
        this.left = left;
        this.right = right;
    }

    public abstract String render();
}
