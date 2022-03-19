package io.github.greyp9.arwo.core.expr.eval;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.hash.secure.HashU;

import java.util.List;

public final class Sha256Evaluator extends Evaluator {

    public Node evaluate(final List<Node> operands) {
        validateSize(1, operands);
        final Operand operand = (Operand) operands.get(0);
        return new Operand(HexCodec.encode(HashU.sha256(UTF8Codec.toBytes(operand.getValue()))));
    }
}
