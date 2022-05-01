package io.github.greyp9.arwo.core.envsec.eval;

import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.Value;

import java.io.File;
import java.io.IOException;
import java.util.List;

public final class Sha256Evaluator extends Evaluator {

    public NameTypeValue evaluate(final MultiOperator multiOperator) throws IOException {
        final List<Node> operands = multiOperator.getOperands();
        Value.require(operands.size() == 2, () ->
                new IOException(String.format("%d != %d", 2, operands.size())));
        final String folderPath = ((Operand) operands.get(0)).getValue();
        final String filename = ((Operand) operands.get(1)).getValue();
        final File folder = (folderPath == null) ? null : new File(SystemU.resolve(folderPath));
        final File file = (folder == null) ? null : new File(folder, filename);
        final byte[] bytes = ((file == null) || (!file.exists()) ? new byte[0] : StreamU.read(file));
        final String value = HexCodec.encode(HashU.sha256(bytes));
        return new NameTypeValue(multiOperator.render(), value);
    }
}
