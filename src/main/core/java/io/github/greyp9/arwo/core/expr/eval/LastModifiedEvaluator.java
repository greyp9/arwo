package io.github.greyp9.arwo.core.expr.eval;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.lang.SystemU;

import java.io.File;
import java.util.List;

public final class LastModifiedEvaluator extends Evaluator {

    public Node evaluate(final List<Node> operands) {
        validateSize(1, operands);
        final Operand operand = (Operand) operands.get(0);
        final File file = new File(operand.getValue().replace("~", SystemU.userHome()));
        return new Operand(Long.toString(file.lastModified()));
    }
}
