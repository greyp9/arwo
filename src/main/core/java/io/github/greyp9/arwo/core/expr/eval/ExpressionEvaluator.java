package io.github.greyp9.arwo.core.expr.eval;

import io.github.greyp9.arwo.core.env.eval.AtomNode;
import io.github.greyp9.arwo.core.expr.Grammar;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.Tree;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public final class ExpressionEvaluator {
    private final Map<String, Evaluator> evaluators;

    public ExpressionEvaluator() {
        this.evaluators = new TreeMap<>();
    }

    public void register(final String name, final Evaluator evaluator) {
        evaluators.put(name, evaluator);
    }

    public String evaluateAsString(final String expression) {
        final Node node = evaluate(expression);
        if (node instanceof Operand) {
            return ((Operand) node).getValue();
        } else if (node instanceof AtomNode) {
            return ((AtomNode) node).getAtom().getValue();
        } else {
            return null;
        }
    }

    public Node evaluate(final String expression) {
        final Grammar grammar = new Grammar(expression);
        final Tree tree = new Tree(grammar.toNode());
        final Node evaluate = evaluate(tree.getRoot());
        return evaluate;
    }

    private Node evaluate(final Node node) {
        final Node nodeA;
        if (node instanceof Operand) {
            nodeA = evaluate((Operand) node);
        } else if (node instanceof MultiOperator) {
            nodeA = evaluate((MultiOperator) node);
        } else {
            throw new IllegalStateException(node.toString());
        }
        return nodeA;
    }

    private Node evaluate(final Operand operand) {
        return operand;
    }

    private Node evaluate(final MultiOperator multiOperator) {
        final String op = multiOperator.getOp();
        final List<Node> operands = multiOperator.getOperands();
        final List<Node> operandsA = new ArrayList<>();
        for (Node operand : operands) {
            operandsA.add(evaluate(operand));
        }
        final Node node;
        final Evaluator evaluator = evaluators.get(op);
        if (evaluator == null) {
            throw new IllegalArgumentException(op);
        } else {
            node = evaluator.evaluate(new MultiOperator(multiOperator.getOp(), operandsA));
        }
        return node;
    }
}
