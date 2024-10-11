package io.github.greyp9.arwo.core.expr.eval;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.env.EnvironmentAtom;
import io.github.greyp9.arwo.core.env.eval.AtomNode;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.hash.secure.HashU;

import java.util.List;

public final class Sha256Evaluator extends Evaluator {

    @Override
    protected Node evaluate(final MultiOperator multiOperator) {
        final List<Node> operands = multiOperator.getOperands();
        validateSize(1, operands);
        //final Operand operand = (Operand) operands.get(0);
        final AtomNode atomNode = (AtomNode) operands.get(0);
        final String operand = atomNode.getAtom().getValue();
        return new AtomNode(new EnvironmentAtom(0, multiOperator.render(),
                HexCodec.encode(HashU.sha256(UTF8Codec.toBytes(operand)))));
    }
}
