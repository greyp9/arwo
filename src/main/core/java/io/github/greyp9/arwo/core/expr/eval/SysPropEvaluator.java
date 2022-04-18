package io.github.greyp9.arwo.core.expr.eval;

import io.github.greyp9.arwo.core.env.EnvironmentAtom;
import io.github.greyp9.arwo.core.env.eval.AtomNode;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;

import java.util.List;

public final class SysPropEvaluator extends Evaluator {

    @Override
    protected Node evaluate(final MultiOperator multiOperator) {
        final List<Node> operands = multiOperator.getOperands();
        validateSize(1, operands);
        final Operand operand = (Operand) operands.get(0);
        return new AtomNode(new EnvironmentAtom(0, multiOperator.render(),
                System.getProperty(operand.getValue())));
    }
}
