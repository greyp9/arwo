package io.github.greyp9.arwo.core.envsec.eval;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.Value;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;

public final class FolderEvaluator extends Evaluator {

    public NameTypeValue evaluate(final MultiOperator multiOperator) throws IOException {
        final List<Node> operands = multiOperator.getOperands();
        Value.require((operands.size() == 2), () ->
                new IOException(String.format("%d != %d", 2, operands.size())));
        final Operand operandFolder = (Operand) operands.get(0);
        final Operand operandPattern = (Operand) operands.get(1);
        final File folder = new File(SystemU.resolve(operandFolder.getValue()));
        final String pattern = operandPattern.getValue();
        final FindInFolderQuery query = new FindInFolderQuery(folder, pattern, false);
        final Collection<File> files = new TreeSet<>(query.getFound());
        final StringBuilder buffer = new StringBuilder();
        for (File file : files) {
            buffer.append(String.format("[%s] [%d] [%d]%n", file.getName(), file.length(), file.lastModified()));
        }
        return new NameTypeValue(multiOperator.render(), buffer.toString());
    }
}
