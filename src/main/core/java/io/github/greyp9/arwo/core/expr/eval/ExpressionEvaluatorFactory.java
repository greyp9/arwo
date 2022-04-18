package io.github.greyp9.arwo.core.expr.eval;

import io.github.greyp9.arwo.core.env.eval.KeyEvaluator;

public final class ExpressionEvaluatorFactory {

    private ExpressionEvaluatorFactory() {
    }

    public static ExpressionEvaluator create() {
        final ExpressionEvaluator evaluator = new ExpressionEvaluator();
        evaluator.register(PROP, new SysPropEvaluator());
        evaluator.register(ENV, new SysEnvEvaluator());
        evaluator.register(MODIFIED, new LastModifiedEvaluator());
        evaluator.register(KEY, new KeyEvaluator());
        return evaluator;
    }

    private static final String PROP = "prop";
    private static final String ENV = "env";
    private static final String MODIFIED = "mod";
    private static final String KEY = "key";
}
