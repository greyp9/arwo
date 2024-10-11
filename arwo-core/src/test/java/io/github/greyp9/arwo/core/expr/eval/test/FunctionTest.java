package io.github.greyp9.arwo.core.expr.eval.test;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.logging.Logger;

public class FunctionTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public final void setUp() throws Exception {
        logger.finest("setup()");
    }

    @Test
    public void testFunctionCreateSimple() {
        final Node nodeA = new Operand("a");
        final Node nodeF = new MultiOperator("f", nodeA);
        Assertions.assertEquals("f(a)", nodeF.render());
    }

    @Test
    public void testFunctionCreateNested() {
        final Node nodeA = new Operand("a");
        final Node nodeF = new MultiOperator("f", nodeA);
        final Node nodeG = new MultiOperator("g", nodeF);
        Assertions.assertEquals("g(f(a))", nodeG.render());
    }

    @Test
    public void testFunctionCreateNestedAlphaNum() {
        final Node nodeA = new Operand("a");
        final Node nodeF = new MultiOperator("f", nodeA);
        final Node nodeG = new MultiOperator("g", nodeF);
        final Node nodeH = new MultiOperator("h2", nodeG);
        Assertions.assertEquals("h2(g(f(a)))", nodeH.render());
    }
}
