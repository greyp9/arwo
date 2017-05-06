package io.github.greyp9.arwo.core.expr.test;

import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.Operator;
import io.github.greyp9.arwo.core.expr.Tree;
import junit.framework.TestCase;
import org.junit.Assert;

public class ExpressionTest extends TestCase {

    public void testSimple() throws Exception {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node node = new Operator(nodeA, nodeB, "OR");
        Assert.assertEquals("(A OR B)", node.render());
    }

    public void testComplex() throws Exception {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node nodeOrAB = new Operator(nodeA, nodeB, "OR");
        final Node nodeC = new Operand("C");
        final Node nodeD = new Operand("D");
        final Node nodeOrCD = new Operator(nodeC, nodeD, "OR");
        final Node node = new Operator(nodeOrAB, nodeOrCD, "AND");
        Assert.assertEquals("((A OR B) AND (C OR D))", node.render());
    }

    public void testEvaluateOrSimple() throws Exception {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node nodeOrAB = new Operator(nodeA, nodeB, "OR");
        final Tree tree = new Tree(nodeOrAB);
        Assert.assertTrue(tree.evaluate("A"));
        Assert.assertTrue(tree.evaluate("B"));
        Assert.assertFalse(tree.evaluate("C"));
    }

    public void testEvaluateAndSimple() throws Exception {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node nodeAndAB = new Operator(nodeA, nodeB, "AND");
        final Tree tree = new Tree(nodeAndAB);
        Assert.assertFalse(tree.evaluate("A"));
        Assert.assertFalse(tree.evaluate("B"));
        Assert.assertFalse(tree.evaluate("C"));
        Assert.assertTrue(tree.evaluate("AB"));
    }

    public void testEvaluateNotSimple() throws Exception {
        final Node nodeA = new Operand("A");
        final Node nodeNotA = new Operator(null, nodeA, "NOT");
        final Tree tree = new Tree(nodeNotA);
        Assert.assertFalse(tree.evaluate("A"));
        Assert.assertTrue(tree.evaluate("B"));
    }

    public void testEvaluateComplex() throws Exception {
        final Node nodeA = new Operand("A");
        final Node nodeB = new Operand("B");
        final Node nodeOrAB = new Operator(nodeA, nodeB, "OR");
        final Node nodeC = new Operand("C");
        final Node nodeD = new Operand("D");
        final Node nodeOrCD = new Operator(nodeC, nodeD, "OR");
        final Node node = new Operator(nodeOrAB, nodeOrCD, "AND");
        final Tree tree = new Tree(node);
        Assert.assertFalse(tree.evaluate("AB"));
        Assert.assertFalse(tree.evaluate("CD"));
        Assert.assertTrue(tree.evaluate("AC"));
        Assert.assertTrue(tree.evaluate("BD"));
    }
}
