package io.github.greyp9.arwo.core.expr.test;

import io.github.greyp9.arwo.core.expr.Grammar;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.text.filter.TextMatchTree;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.logging.Logger;

public class GrammarTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private void checkTree(final TextMatchTree tree) {
        Assertions.assertNotNull(tree);
        final Node node = tree.getRoot();
        Assertions.assertNotNull(node);
        logger.fine(node.render());
        Assertions.assertTrue(node instanceof Operand);
        final Operand operand = (Operand) node;
        Assertions.assertEquals("A", operand.getValue());
        Assertions.assertTrue(tree.evaluate("A"));
        Assertions.assertFalse(tree.evaluate("B"));
        Assertions.assertTrue(tree.evaluate("AB"));
    }

    @Test
    public void testGrammar1() {
        final Grammar grammar = new Grammar("A");
        final Node node = grammar.toNode();
        Assertions.assertNotNull(node);
        checkTree(new TextMatchTree(node));
        Assertions.assertEquals("A", node.render());
    }

    @Test
    public void testGrammar2() {
        final Grammar grammar = new Grammar("'A'");
        final Node node = grammar.toNode();
        Assertions.assertNotNull(node);
        checkTree(new TextMatchTree(grammar.toNode()));
        Assertions.assertEquals("A", node.render());
    }

    @Test
    public void testGrammar3() {
        final Grammar grammar = new Grammar("\"A\"");
        final Node node = grammar.toNode();
        Assertions.assertNotNull(node);
        checkTree(new TextMatchTree(grammar.toNode()));
        Assertions.assertEquals("A", node.render());
    }

    @Test
    public void testGrammar4() {
        final Grammar grammar = new Grammar("(A)");
        final Node node = grammar.toNode();
        Assertions.assertNotNull(node);
        checkTree(new TextMatchTree(grammar.toNode()));
        Assertions.assertEquals("A", node.render());
    }

    @Test
    public void testGrammar5() {
        final Grammar grammar = new Grammar("('A')");
        final Node node = grammar.toNode();
        Assertions.assertNotNull(node);
        checkTree(new TextMatchTree(grammar.toNode()));
        Assertions.assertEquals("A", node.render());
    }

    @Test
    public void testGrammar6() {
        final Grammar grammar = new Grammar("(\"A\")");
        final Node node = grammar.toNode();
        Assertions.assertNotNull(node);
        checkTree(new TextMatchTree(grammar.toNode()));
        Assertions.assertEquals("A", node.render());
    }

    @Test
    public void testGrammarAND1() {
        final Grammar grammar = new Grammar("A AND B");
        final TextMatchTree tree = new TextMatchTree(grammar.toNode());
        Assertions.assertFalse(tree.evaluate("A"));
        Assertions.assertFalse(tree.evaluate("B"));
        Assertions.assertTrue(tree.evaluate("AB"));
        Assertions.assertEquals("(A AND B)", tree.getRoot().render());
    }

    @Test
    public void testGrammarAND2() {
        final Grammar grammar = new Grammar("(A AND B)");
        final TextMatchTree tree = new TextMatchTree(grammar.toNode());
        Assertions.assertFalse(tree.evaluate("A"));
        Assertions.assertFalse(tree.evaluate("B"));
        Assertions.assertTrue(tree.evaluate("AB"));
        Assertions.assertEquals("(A AND B)", tree.getRoot().render());
    }

    @Test
    public void testGrammarOR1() {
        final Grammar grammar = new Grammar("A OR B");
        final TextMatchTree tree = new TextMatchTree(grammar.toNode());
        Assertions.assertTrue(tree.evaluate("A"));
        Assertions.assertTrue(tree.evaluate("B"));
        Assertions.assertTrue(tree.evaluate("AB"));
        Assertions.assertEquals("(A OR B)", tree.getRoot().render());
    }

    @Test
    public void testGrammarOR2() {
        final Grammar grammar = new Grammar("(A OR B)");
        final TextMatchTree tree = new TextMatchTree(grammar.toNode());
        Assertions.assertTrue(tree.evaluate("A"));
        Assertions.assertTrue(tree.evaluate("B"));
        Assertions.assertTrue(tree.evaluate("AB"));
        Assertions.assertEquals("(A OR B)", tree.getRoot().render());
    }

    @Test
    public void testGrammarNOT1() {
        final Grammar grammar = new Grammar("NOT A");
        final TextMatchTree tree = new TextMatchTree(grammar.toNode());
        Assertions.assertFalse(tree.evaluate("A"));
        Assertions.assertTrue(tree.evaluate("B"));
        Assertions.assertEquals("(NOT A)", tree.getRoot().render());
    }

    @Test
    public void testGrammarNOT2() {
        final Grammar grammar = new Grammar("(NOT A)");
        final TextMatchTree tree = new TextMatchTree(grammar.toNode());
        Assertions.assertFalse(tree.evaluate("A"));
        Assertions.assertTrue(tree.evaluate("B"));
        Assertions.assertEquals("(NOT A)", tree.getRoot().render());
    }

    @Test
    public void testGrammarNOT3() {
        final Grammar grammar = new Grammar("NOT NOT A");
        final TextMatchTree tree = new TextMatchTree(grammar.toNode());
        Assertions.assertTrue(tree.evaluate("A"));
        Assertions.assertFalse(tree.evaluate("B"));
        Assertions.assertEquals("(NOT (NOT A))", tree.getRoot().render());
    }

    @Test
    public void testGrammarInvalid1() {
        final Grammar grammar = new Grammar("('A' 'OR' 'B')");
        final IllegalArgumentException exception = Assertions.assertThrows(
                IllegalArgumentException.class, grammar::toNode);
        Assertions.assertEquals("OR", exception.getMessage());
    }

    @Test
    public void testGrammarFnSimple() {
        final Grammar grammar = new Grammar("f(x)");
        final Node root = grammar.toNode();
        Assertions.assertNotNull(root);
        Assertions.assertEquals("f(x)", root.render());
    }

    @Test
    public void testGrammarFnMultiArg() {
        final Grammar grammar = new Grammar("f(x y z)");
        final Node root = grammar.toNode();
        Assertions.assertNotNull(root);
        Assertions.assertEquals("f(x y z)", root.render());
    }

    @Test
    public void testGrammarFnNested() {
        final Grammar grammar = new Grammar("f(g(x))");
        final Node root = grammar.toNode();
        Assertions.assertNotNull(root);
        Assertions.assertEquals("f(g(x))", root.render());
    }

    @Test
    public void testGrammarFnMultiArgNested() {
        final Grammar grammar = new Grammar("f(g('x') h(y) i(z))");
        final Node root = grammar.toNode();
        Assertions.assertNotNull(root);
        Assertions.assertEquals("f(g(x) h(y) i(z))", root.render());
    }

    @Test
    public void testGrammarFnInvalid() {
        final Grammar grammar = new Grammar("'f'(x)");
        final Node root = grammar.toNode();
        Assertions.assertNull(root);
    }
}
