package io.github.greyp9.arwo.core.expr.eval;

import io.github.greyp9.arwo.core.expr.Node;

import java.util.List;

public abstract class Evaluator {

    abstract Node evaluate(List<Node> operands);

    protected final void validateSize(final int size, final List<Node> nodes) {
        if (size != nodes.size()) {
            throw new IllegalArgumentException(String.format("Expected=%d, Actual=%d", size, nodes.size()));
        }
    }
}
