package io.github.greyp9.arwo.core.envsec.eval;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;
import java.util.List;
import java.util.Properties;

public final class PropEvaluator extends Evaluator {
    private final Properties properties;

    public PropEvaluator(final Properties properties) {
        this.properties = properties;
    }

    public NameTypeValue evaluate(final MultiOperator multiOperator) throws IOException {
        final List<Node> operands = multiOperator.getOperands();
        Value.require(operands.size() == 1, () ->
                new IOException(String.format("%d != %d", 1, operands.size())));
        final Operand operand = (Operand) operands.get(0);
        final String key = operand.getValue();
        return new NameTypeValue(multiOperator.render(), properties.getProperty(key));
    }
}
