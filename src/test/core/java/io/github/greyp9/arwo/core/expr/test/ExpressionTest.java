package io.github.greyp9.arwo.core.expr.test;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.op.BinaryOperator;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.expr.op.UnaryOperator;
import io.github.greyp9.arwo.core.text.filter.TextMatchTree;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.logging.Logger;

public class ExpressionTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public final void setUp() throws Exception {
        logger.finest("setup()");
    }

    @Test
    public void testSimple() {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node node = new BinaryOperator("OR", nodeA, nodeB);
        Assertions.assertEquals("(A OR B)", node.render());
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
        Assertions.assertEquals("((A OR B) AND (C OR D))", node.render());
        final TextMatchTree tree = new TextMatchTree(node);
        Assertions.assertFalse(tree.evaluate("AB"));
        Assertions.assertFalse(tree.evaluate("CD"));
        Assertions.assertTrue(tree.evaluate("AC"));
        Assertions.assertTrue(tree.evaluate("BD"));
    }

    @Test
    public void testEvaluateOrSimple() {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node nodeOrAB = new BinaryOperator("OR", nodeA, nodeB);
        final TextMatchTree tree = new TextMatchTree(nodeOrAB);
        Assertions.assertTrue(tree.evaluate("A"));
        Assertions.assertTrue(tree.evaluate("B"));
        Assertions.assertFalse(tree.evaluate("C"));
    }

    @Test
    public void testEvaluateAndSimple() {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node nodeAndAB = new BinaryOperator("AND", nodeA, nodeB);
        final TextMatchTree tree = new TextMatchTree(nodeAndAB);
        Assertions.assertFalse(tree.evaluate("A"));
        Assertions.assertFalse(tree.evaluate("B"));
        Assertions.assertFalse(tree.evaluate("C"));
        Assertions.assertTrue(tree.evaluate("AB"));
    }

    @Test
    public void testEvaluateNotSimple() {
        final Node nodeA = new Operand("A");
        final Node nodeNotA = new UnaryOperator("NOT", nodeA);
        final TextMatchTree tree = new TextMatchTree(nodeNotA);
        Assertions.assertFalse(tree.evaluate("A"));
        Assertions.assertTrue(tree.evaluate("B"));
    }

    @Test
    public void testUnaryOperator() {
        final Node nodeA = new Operand("A");
        final Node node = new UnaryOperator("NOT", nodeA);
        Assertions.assertEquals("(NOT A)", node.render());
    }

    @Test
    public void testBinaryOperator() {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node node = new BinaryOperator("OR", nodeA, nodeB);
        Assertions.assertEquals("(A OR B)", node.render());
    }

    @Test
    public void testMultiOperator() {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node nodeC = new Operand("C");
        final Node nodeD = new Operand("D");
        final Node node = new MultiOperator("f", nodeA, nodeB, nodeC, nodeD);
        Assertions.assertEquals("f(A B C D)", node.render());
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
        Assertions.assertEquals("(f(A B) AND (C OR D))", node.render());
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
        Assertions.assertEquals("((A OR B) AND (C OR D))", node.render());
    }
}
