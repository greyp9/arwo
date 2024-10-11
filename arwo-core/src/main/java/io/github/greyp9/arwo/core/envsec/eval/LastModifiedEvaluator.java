package io.github.greyp9.arwo.core.envsec.eval;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.Value;

import java.io.File;
import java.io.IOException;
import java.util.List;

public final class LastModifiedEvaluator extends Evaluator {

    public NameTypeValue evaluate(final MultiOperator multiOperator) throws IOException {
        final List<Node> operands = multiOperator.getOperands();
        Value.require(operands.size() == 1, () ->
                new IOException(String.format("%d != %d", 1, operands.size())));
        final Operand operand = (Operand) operands.get(0);
        final String path = operand.getValue();
        final File file = new File(SystemU.resolve(path));
        final String value = (file.exists() ? Long.toString(file.lastModified()) : null);
        return new NameTypeValue(multiOperator.render(), value);
    }
}
