package io.github.greyp9.arwo.core.expr.eval;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;

import java.util.List;

public final class SysPropEvaluator extends Evaluator {

    public Node evaluate(final List<Node> operands) {
        validateSize(1, operands);
        final Operand operand = (Operand) operands.get(0);
        return new Operand(System.getProperty(operand.getValue()));
    }
}
