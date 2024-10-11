package io.github.greyp9.arwo.core.envsec.eval;

import io.github.greyp9.arwo.core.expr.op.MultiOperator;

import java.io.IOException;

public abstract class Evaluator {

    abstract Object evaluate(MultiOperator multiOperator) throws IOException;
}
