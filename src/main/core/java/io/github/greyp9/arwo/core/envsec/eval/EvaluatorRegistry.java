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
        this.evaluators.put(Const.ENV, new SysEnvEvaluator());
        this.evaluators.put(Const.FOLDER, new FolderEvaluator());
        this.evaluators.put(Const.MOD, new LastModifiedEvaluator());
        this.evaluators.put(Const.PROP, new SysPropEvaluator());
        this.evaluators.put(Const.SHA256, new Sha256Evaluator());
    }

    public void register(final String key, final Evaluator evaluator) throws IOException {
        if (evaluators.containsKey(key)) {
            throw new IOException(key, new IllegalStateException(key));
        } else {
            evaluators.put(key, evaluator);
        }
    }

    public NameTypeValue evaluate(final MultiOperator operator) throws IOException {
        final String op = operator.getOp();
        final List<Node> operands = new ArrayList<>();
        for (Node node : operator.getOperands()) {
            operands.add(toOperand(node));
        }
        final Evaluator evaluator = Optional.ofNullable(evaluators.get(op))
                .orElseThrow(() -> new IOException(op));
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

    private static class Const {
        private static final String ENV = "env";
        private static final String FOLDER = "folder";
        private static final String MOD = "mod";
        private static final String PROP = "prop";
        private static final String SHA256 = "sha256";
    }
}
