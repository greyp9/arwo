package io.github.greyp9.arwo.core.expr.eval.test;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.logging.Logger;

public class FunctionTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        logger.finest("setup()");
    }

    @Test
    public void testFunctionCreateSimple() {
        final Node nodeA = new Operand("a");
        final Node nodeF = new MultiOperator("f", nodeA);
        Assert.assertEquals("f(a)", nodeF.render());
    }

    @Test
    public void testFunctionCreateNested() {
        final Node nodeA = new Operand("a");
        final Node nodeF = new MultiOperator("f", nodeA);
        final Node nodeG = new MultiOperator("g", nodeF);
        Assert.assertEquals("g(f(a))", nodeG.render());
    }

    @Test
    public void testFunctionCreateNestedAlphanum() {
        final Node nodeA = new Operand("a");
        final Node nodeF = new MultiOperator("f", nodeA);
        final Node nodeG = new MultiOperator("g", nodeF);
        final Node nodeH = new MultiOperator("h2", nodeG);
        Assert.assertEquals("h2(g(f(a)))", nodeH.render());
    }
}
