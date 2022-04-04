package io.github.greyp9.arwo.core.env;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.eval.Evaluator;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

public final class KeyEvaluator extends Evaluator {

    public Node evaluate(final List<Node> operands) {
        final List<Node> operandsLocal = new ArrayList<>(operands);
        final Operand operandThresholdCount = (Operand) operandsLocal.remove(0);
        final int thresholdCount = Integer.parseInt(operandThresholdCount.getValue());
        final int shareCount = operandsLocal.size();
        Logger.getLogger(getClass().getName()).finest(
                String.format("evaluate():THRESHOLD=%d,SHARES=%d", thresholdCount, shareCount));
        return null;
    }
}
