package io.github.greyp9.arwo.core.expr;

public final class Tree {
    private final Node root;

    public Node getRoot() {
        return root;
    }

    public Tree(final Node root) {
        this.root = root;
    }
}
