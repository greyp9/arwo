package io.github.greyp9.arwo.core.expr.eval;

import io.github.greyp9.arwo.core.env.EnvironmentAtom;
import io.github.greyp9.arwo.core.env.eval.AtomNode;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.lang.SystemU;

import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;

public final class FolderEvaluator extends Evaluator {

    @Override
    protected Node evaluate(final MultiOperator multiOperator) {
        final List<Node> operands = multiOperator.getOperands();
        validateSize(2, operands);
        //final Operand operand = (Operand) operands.get(0);
        final AtomNode atomNode = (AtomNode) operands.get(0);
        final String operand0 = atomNode.getAtom().getValue();
        final Operand operand1 = (Operand) operands.get(1);
        final File folder = new File(operand0.replace("~", SystemU.userHome()));
        final FindInFolderQuery query = new FindInFolderQuery(folder, operand1.getValue(), false);
        final Collection<File> files = new TreeSet<>(query.getFound());
        final StringBuilder buffer = new StringBuilder();
        for (File file : files) {
            buffer.append(String.format("[%s] [%d] [%d]%n", file.getName(), file.length(), file.lastModified()));
        }
        return new AtomNode(new EnvironmentAtom(0, multiOperator.render(),
                buffer.toString()));
    }
}
