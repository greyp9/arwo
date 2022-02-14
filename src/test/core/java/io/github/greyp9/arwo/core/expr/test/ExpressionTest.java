package io.github.greyp9.arwo.core.expr.test;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.BinaryOperator;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.expr.op.UnaryOperator;
import io.github.greyp9.arwo.core.text.filter.TextMatchTree;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.logging.Logger;

public class ExpressionTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        logger.finest("setup()");
    }

    @Test
    public void testSimple() {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node node = new BinaryOperator("OR", nodeA, nodeB);
        Assert.assertEquals("(A OR B)", node.render());
    }

    @Test
    public void testEvaluateComplex() {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node nodeOrAB = new BinaryOperator("OR", nodeA, nodeB);
        final Node nodeC = new Operand("C");
        final Node nodeD = new Operand("D");
        final Node nodeOrCD = new BinaryOperator("OR", nodeC, nodeD);
        final Node node = new BinaryOperator("AND", nodeOrAB, nodeOrCD);
        Assert.assertEquals("((A OR B) AND (C OR D))", node.render());
        final TextMatchTree tree = new TextMatchTree(node);
        Assert.assertFalse(tree.evaluate("AB"));
        Assert.assertFalse(tree.evaluate("CD"));
        Assert.assertTrue(tree.evaluate("AC"));
        Assert.assertTrue(tree.evaluate("BD"));
    }

    @Test
    public void testEvaluateOrSimple() {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node nodeOrAB = new BinaryOperator("OR", nodeA, nodeB);
        final TextMatchTree tree = new TextMatchTree(nodeOrAB);
        Assert.assertTrue(tree.evaluate("A"));
        Assert.assertTrue(tree.evaluate("B"));
        Assert.assertFalse(tree.evaluate("C"));
    }

    @Test
    public void testEvaluateAndSimple() {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node nodeAndAB = new BinaryOperator("AND", nodeA, nodeB);
        final TextMatchTree tree = new TextMatchTree(nodeAndAB);
        Assert.assertFalse(tree.evaluate("A"));
        Assert.assertFalse(tree.evaluate("B"));
        Assert.assertFalse(tree.evaluate("C"));
        Assert.assertTrue(tree.evaluate("AB"));
    }

    @Test
    public void testEvaluateNotSimple() {
        final Node nodeA = new Operand("A");
        final Node nodeNotA = new UnaryOperator("NOT", nodeA);
        final TextMatchTree tree = new TextMatchTree(nodeNotA);
        Assert.assertFalse(tree.evaluate("A"));
        Assert.assertTrue(tree.evaluate("B"));
    }

    @Test
    public void testUnaryOperator() {
        final Node nodeA = new Operand("A");
        final Node node = new UnaryOperator("NOT", nodeA);
        Assert.assertEquals("(NOT A)", node.render());
    }

    @Test
    public void testBinaryOperator() {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node node = new BinaryOperator("OR", nodeA, nodeB);
        Assert.assertEquals("(A OR B)", node.render());
    }

    @Test
    public void testMultiOperator() {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node nodeC = new Operand("C");
        final Node nodeD = new Operand("D");
        final Node node = new MultiOperator("f", nodeA, nodeB, nodeC, nodeD);
        Assert.assertEquals("f(A B C D)", node.render());
    }

    @Test
    public void testMultiOperatorComplex() {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node nodeC = new Operand("C");
        final Node nodeD = new Operand("D");
        final Node nodeAB = new MultiOperator("f", nodeA, nodeB);
        final Node nodeCD = new BinaryOperator("OR", nodeC, nodeD);
        final Node node = new BinaryOperator("AND", nodeAB, nodeCD);
        Assert.assertEquals("(f(A B) AND (C OR D))", node.render());
    }

    @Test
    public void testComplex() {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node nodeOrAB = new BinaryOperator("OR", nodeA, nodeB);
        final Node nodeC = new Operand("C");
        final Node nodeD = new Operand("D");
        final Node nodeOrCD = new BinaryOperator("OR", nodeC, nodeD);
        final Node node = new BinaryOperator("AND", nodeOrAB, nodeOrCD);
        Assert.assertEquals("((A OR B) AND (C OR D))", node.render());
    }
}
