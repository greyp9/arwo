package io.github.greyp9.arwo.core.envsec.eval;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.value.NameTypeValue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

public final class EvaluatorRegistry {
    private final Map<String, Evaluator> evaluators;

    public EvaluatorRegistry() {
        this.evaluators = new TreeMap<>();
        this.evaluators.put("prop", new SysPropEvaluator());
        this.evaluators.put("env", new SysEnvEvaluator());
        this.evaluators.put("mod", new LastModifiedEvaluator());
    }

    public NameTypeValue evaluate(final MultiOperator operator) throws IOException {
        final String op = operator.getOp();
        final List<Node> operands = new ArrayList<>();
        for (Node node : operator.getOperands()) {
            operands.add(toOperand(node));
        }
        final Evaluator evaluator = Optional.ofNullable(evaluators.get(op))
                .orElseThrow(() -> new IOException("foo"));
        return (NameTypeValue) evaluator.evaluate(new MultiOperator(op, operands));
    }

    private Operand toOperand(final Node node) throws IOException {
        final Operand operand;
        if (node instanceof Operand) {
            operand = (Operand) node;
        } else if (node instanceof MultiOperator) {
            final NameTypeValue ntv = evaluate((MultiOperator) node);
            operand = new Operand(ntv.getValueS());
        } else {
            throw new IOException(node.getClass().getName());
        }
        return operand;
    }
}
