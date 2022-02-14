package io.github.greyp9.arwo.core.expr.test;

import io.github.greyp9.arwo.core.expr.token.Parser;
import org.junit.Assert;
import org.junit.Test;

public class ParserTest {

    @Test
    public void testEmpty() {
        final Parser parser = new Parser("");
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testParenthesesEmpty() {
        final Parser parser = new Parser("()");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testParenthesesEmbed() {
        final Parser parser = new Parser("(())");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testParenthesesSimple() {
        final Parser parser = new Parser("(A)");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("A", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testParenthesesSingleQuote() {
        final Parser parser = new Parser("('A')");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("A", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testParenthesesDoubleQuote() {
        final Parser parser = new Parser("(\"A\")");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("A", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testStringSingleQuote() {
        final Parser parser = new Parser("'hello'");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("hello", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testStringDoubleQuote() {
        final Parser parser = new Parser("\"hello\"");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("hello", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testWord() {
        final Parser parser = new Parser("hello");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("hello", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testWords() {
        final Parser parser = new Parser("hello OR goodbye");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("hello", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("OR", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("goodbye", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testWhitespace() {
        final Parser parser = new Parser("  hello   OR     goodbye   ");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("hello", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("OR", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("goodbye", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testFunctionSimple() {
        final Parser parser = new Parser("f(x)");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("f", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("x", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testFunctionQuoted() {
        final Parser parser = new Parser("f('x y')");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("f", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("x y", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testFunctionMultiArg() {
        final Parser parser = new Parser("f(x y z)");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("f", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("x", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("y", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("z", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testFunctionMultiArgQuoted() {
        final Parser parser = new Parser("f('x x' 'y y' z)");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("f", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("x x", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("y y", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("z", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testFunctionNested() {
        final Parser parser = new Parser("f(g(x))");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("f", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("g", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("x", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testFunctionComplex() {
        final Parser parser = new Parser("f(x) AND xyz");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("f", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("x", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("AND", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("xyz", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }
}
