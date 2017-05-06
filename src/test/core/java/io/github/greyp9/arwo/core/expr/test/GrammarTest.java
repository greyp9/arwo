package io.github.greyp9.arwo.core.expr.test;

import io.github.greyp9.arwo.core.expr.Grammar;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.Tree;
import junit.framework.TestCase;
import org.junit.Assert;

public class GrammarTest extends TestCase {

    private void checkTree(Tree tree) {
        Assert.assertNotNull(tree);
        final Node node = tree.getRoot();
        Assert.assertNotNull(node);
        Assert.assertTrue(node instanceof Operand);
        final Operand operand = (Operand) node;
        Assert.assertEquals("A", operand.getValue());
        Assert.assertNull(operand.getLeft());
        Assert.assertNull(operand.getRight());
        Assert.assertTrue(tree.evaluate("A"));
        Assert.assertFalse(tree.evaluate("B"));
        Assert.assertTrue(tree.evaluate("AB"));
    }

    public void testGrammar1() throws Exception {
        final Grammar grammar = new Grammar("A");
        final Tree tree = grammar.createTree();
        checkTree(tree);
    }

    public void testGrammar2() throws Exception {
        final Grammar grammar = new Grammar("'A'");
        final Tree tree = grammar.createTree();
        checkTree(tree);
    }

    public void testGrammar3() throws Exception {
        final Grammar grammar = new Grammar("\"A\"");
        final Tree tree = grammar.createTree();
        checkTree(tree);
    }

    public void testGrammar4() throws Exception {
        final Grammar grammar = new Grammar("(A)");
        final Tree tree = grammar.createTree();
        checkTree(tree);
    }

    public void testGrammar5() throws Exception {
        final Grammar grammar = new Grammar("('A')");
        final Tree tree = grammar.createTree();
        checkTree(tree);
    }

    public void testGrammar6() throws Exception {
        final Grammar grammar = new Grammar("(\"A\")");
        final Tree tree = grammar.createTree();
        checkTree(tree);
    }

    public void testGrammarAND1() throws Exception {
        final Grammar grammar = new Grammar("A AND B");
        final Tree tree = grammar.createTree();
        Assert.assertFalse(tree.evaluate("A"));
        Assert.assertFalse(tree.evaluate("B"));
        Assert.assertTrue(tree.evaluate("AB"));
    }

    public void testGrammarAND2() throws Exception {
        final Grammar grammar = new Grammar("(A AND B)");
        final Tree tree = grammar.createTree();
        Assert.assertFalse(tree.evaluate("A"));
        Assert.assertFalse(tree.evaluate("B"));
        Assert.assertTrue(tree.evaluate("AB"));
    }

    public void testGrammarOR1() throws Exception {
        final Grammar grammar = new Grammar("A OR B");
        final Tree tree = grammar.createTree();
        Assert.assertTrue(tree.evaluate("A"));
        Assert.assertTrue(tree.evaluate("B"));
        Assert.assertTrue(tree.evaluate("AB"));
    }

    public void testGrammarOR2() throws Exception {
        final Grammar grammar = new Grammar("(A OR B)");
        final Tree tree = grammar.createTree();
        Assert.assertTrue(tree.evaluate("A"));
        Assert.assertTrue(tree.evaluate("B"));
        Assert.assertTrue(tree.evaluate("AB"));
    }

    public void testGrammarNOT1() throws Exception {
        final Grammar grammar = new Grammar("NOT A");
        final Tree tree = grammar.createTree();
        Assert.assertFalse(tree.evaluate("A"));
        Assert.assertTrue(tree.evaluate("B"));
    }

    public void testGrammarNOT2() throws Exception {
        final Grammar grammar = new Grammar("(NOT A)");
        final Tree tree = grammar.createTree();
        Assert.assertFalse(tree.evaluate("A"));
        Assert.assertTrue(tree.evaluate("B"));
    }

    public void testGrammarInvalid1() throws Exception {
        final Grammar grammar = new Grammar("('A' 'OR' 'B')");
        try {
            grammar.createTree();
            Assert.assertFalse(true);
        } catch (IllegalArgumentException e) {
            Assert.assertEquals("OR", e.getMessage());
        }
    }
}
