package io.github.greyp9.arwo.core.expr.test;

import io.github.greyp9.arwo.core.expr.Parser;
import org.junit.Assert;
import org.junit.Test;

public class ParserTest {

    @Test
    public void testEmpty() throws Exception {
        final Parser parser = new Parser("");
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testParenthesesEmpty() throws Exception {
        final Parser parser = new Parser("()");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testParenthesesEmbed() throws Exception {
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
    public void testParenthesesSimple() throws Exception {
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
    public void testParenthesesSingleQuote() throws Exception {
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
    public void testParenthesesDoubleQuote() throws Exception {
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
    public void testStringSingleQuote() throws Exception {
        final Parser parser = new Parser("'hello'");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("hello", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testStringDoubleQuote() throws Exception {
        final Parser parser = new Parser("\"hello\"");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("hello", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testWord() throws Exception {
        final Parser parser = new Parser("hello");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("hello", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    @Test
    public void testWords() throws Exception {
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
    public void testWhitespace() throws Exception {
        final Parser parser = new Parser("  hello   OR     goodbye   ");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("hello", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("OR", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("goodbye", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }
}
