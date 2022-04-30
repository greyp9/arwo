package io.github.greyp9.arwo.core.envsec.eval;

import io.github.greyp9.arwo.core.expr.op.MultiOperator;

import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

public final class EvaluatorRegistry {

    private final Map<String, Evaluator> evaluators;

    public EvaluatorRegistry() {
        this.evaluators = new TreeMap<>();
        this.evaluators.put("prop", new SysPropEvaluator());
    }

    public Object evaluate(final MultiOperator multiOperator) throws IOException {
        final String op = multiOperator.getOp();
        final Evaluator evaluator = evaluators.get(op);
        if (evaluator == null) {
            throw new IllegalStateException(op);
        } else {
            final Object evaluate = evaluator.evaluate(multiOperator);
            multiOperator.setResult(evaluate);
            return evaluate;
        }
    }
}
