package io.github.greyp9.arwo.core.expr.test;

import io.github.greyp9.arwo.core.expr.Parser;
import junit.framework.TestCase;
import org.junit.Assert;

public class ParserTest extends TestCase {

    public void testEmpty() throws Exception {
        final Parser parser = new Parser("");
        Assert.assertTrue(parser.isDone());
    }

    public void testParenthesesEmpty() throws Exception {
        final Parser parser = new Parser("()");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("(", parser.getNextToken().getValue());
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals(")", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

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

    public void testStringSingleQuote() throws Exception {
        final Parser parser = new Parser("'hello'");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("hello", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    public void testStringDoubleQuote() throws Exception {
        final Parser parser = new Parser("\"hello\"");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("hello", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

    public void testWord() throws Exception {
        final Parser parser = new Parser("hello");
        Assert.assertFalse(parser.isDone());
        Assert.assertEquals("hello", parser.getNextToken().getValue());
        Assert.assertTrue(parser.isDone());
    }

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
