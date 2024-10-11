package io.github.greyp9.arwo.core.expr.test;

import io.github.greyp9.arwo.core.expr.token.Parser;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class ParserTest {

    @Test
    public void testEmpty() {
        final Parser parser = new Parser("");
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testParenthesesEmpty() {
        final Parser parser = new Parser("()");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("(", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals(")", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testParenthesesEmbed() {
        final Parser parser = new Parser("(())");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("(", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("(", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals(")", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals(")", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testParenthesesSimple() {
        final Parser parser = new Parser("(A)");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("(", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("A", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals(")", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testParenthesesSingleQuote() {
        final Parser parser = new Parser("('A')");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("(", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("A", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals(")", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testParenthesesDoubleQuote() {
        final Parser parser = new Parser("(\"A\")");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("(", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("A", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals(")", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testStringSingleQuote() {
        final Parser parser = new Parser("'hello'");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("hello", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testStringDoubleQuote() {
        final Parser parser = new Parser("\"hello\"");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("hello", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testWord() {
        final Parser parser = new Parser("hello");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("hello", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testWords() {
        final Parser parser = new Parser("hello OR goodbye");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("hello", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("OR", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("goodbye", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testWhitespace() {
        final Parser parser = new Parser("  hello   OR     goodbye   ");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("hello", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("OR", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("goodbye", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testFunctionSimple() {
        final Parser parser = new Parser("f(x)");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("f", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("(", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("x", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals(")", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testFunctionQuoted() {
        final Parser parser = new Parser("f('x y')");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("f", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("(", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("x y", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals(")", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testFunctionMultiArg() {
        final Parser parser = new Parser("f(x y z)");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("f", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("(", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("x", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("y", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("z", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals(")", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testFunctionMultiArgQuoted() {
        final Parser parser = new Parser("f('x x' 'y y' z)");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("f", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("(", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("x x", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("y y", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("z", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals(")", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testFunctionNested() {
        final Parser parser = new Parser("f(g(x))");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("f", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("(", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("g", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("(", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("x", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals(")", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals(")", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }

    @Test
    public void testFunctionComplex() {
        final Parser parser = new Parser("f(x) AND xyz");
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("f", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("(", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("x", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals(")", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("AND", parser.getNextToken().getValue());
        Assertions.assertFalse(parser.isDone());
        Assertions.assertEquals("xyz", parser.getNextToken().getValue());
        Assertions.assertTrue(parser.isDone());
    }
}
