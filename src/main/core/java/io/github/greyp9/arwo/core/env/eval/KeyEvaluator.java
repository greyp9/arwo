package io.github.greyp9.arwo.core.env.eval;

import io.github.greyp9.arwo.core.env.EnvironmentAtom;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.eval.Evaluator;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public final class KeyEvaluator extends Evaluator {

    @Override
    protected Node evaluate(final MultiOperator multiOperator) {
        final List<Node> operands = multiOperator.getOperands();
        final List<Node> operandsLocal = new ArrayList<>(operands);
        final Operand operandThresholdCount = (Operand) operandsLocal.remove(0);
        final int thresholdCount = Integer.parseInt(operandThresholdCount.getValue());
        int index = -1;
        final List<AtomNode> collect = operandsLocal.stream()
                .map(o -> (AtomNode) o)
                .collect(Collectors.toList());
        final List<AtomNode> listNew = new ArrayList<>();
        for (AtomNode node : collect) {
            final EnvironmentAtom atom = node.getAtom();
            final EnvironmentAtom atomLocal = new EnvironmentAtom(++index, atom.getKey(), atom.getValue());
            listNew.add(new AtomNode(atomLocal));
        }
        return new KeyNode(thresholdCount, listNew);
    }
}
