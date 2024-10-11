package io.github.greyp9.arwo.core.expr.op;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.value.Value;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public final class MultiOperator extends Node {
    private final String op;
    private final List<Node> operands;

    public String getOp() {
        return op;
    }

    public List<Node> getOperands() {
        return operands;
    }

    public MultiOperator(final String op, final Node... operands) {
        this.op = op;
        this.operands = Arrays.asList(operands);
    }

    public MultiOperator(final String op, final List<Node> operands) {
        this.op = op;
        this.operands = operands;
    }

    @Override
    public String render() {
        return String.format("%s(%s)", op, Value.joinCollection(" ",
                operands.stream().map(Node::render).collect(Collectors.toList())));
    }
}
