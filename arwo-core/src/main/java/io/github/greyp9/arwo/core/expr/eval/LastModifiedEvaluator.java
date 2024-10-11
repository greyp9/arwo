package io.github.greyp9.arwo.core.expr.eval;

import io.github.greyp9.arwo.core.env.EnvironmentAtom;
import io.github.greyp9.arwo.core.env.eval.AtomNode;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.lang.SystemU;

import java.io.File;
import java.util.List;

public final class LastModifiedEvaluator extends Evaluator {

    @Override
    protected Node evaluate(final MultiOperator multiOperator) {
        final List<Node> operands = multiOperator.getOperands();
        validateSize(1, operands);
        final String value;
        final Node node = operands.get(0);
        if (node instanceof Operand) {
            value = ((Operand) node).getValue();
        } else if (node instanceof AtomNode) {
            value = ((AtomNode) node).getAtom().getValue();
        } else {
            throw new IllegalStateException(node.getClass().getName());
        }
        final File file = new File(value.replace("~", SystemU.userHome()));
        return new AtomNode(new EnvironmentAtom(0, multiOperator.render(),
                Long.toString(file.lastModified())));
    }
}
