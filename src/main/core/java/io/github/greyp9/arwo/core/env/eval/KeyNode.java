package io.github.greyp9.arwo.core.env.eval;

import io.github.greyp9.arwo.core.expr.Node;

import java.util.List;

public final class KeyNode extends Node {
    private final int threshold;
    private final List<AtomNode> atomNodes;

    public int getThreshold() {
        return threshold;
    }

    public List<AtomNode> getAtomNodes() {
        return atomNodes;
    }

    public KeyNode(final int threshold, final List<AtomNode> listNew) {
        this.threshold = threshold;
        this.atomNodes = listNew;
    }

    @Override
    public String render() {
        return String.format("key(threshold=%d, size=%d)", threshold, atomNodes.size());
    }
}
