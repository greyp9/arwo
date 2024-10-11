package io.github.greyp9.arwo.core.expr;

import io.github.greyp9.arwo.core.expr.op.BinaryOperator;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.expr.op.UnaryOperator;
import io.github.greyp9.arwo.core.expr.token.Parser;
import io.github.greyp9.arwo.core.expr.token.Token;

import java.util.ArrayList;
import java.util.List;

import static io.github.greyp9.arwo.core.expr.token.Token.Const.AND;
import static io.github.greyp9.arwo.core.expr.token.Token.Const.NOT;
import static io.github.greyp9.arwo.core.expr.token.Token.Const.OR;

public final class Grammar {
    private final String expression;

    public Grammar(final String expression) {
        this.expression = expression;
    }

    public Node toNode() {
        final Parser parser = new Parser(expression);
        final Node root = applyS(parser);
        final boolean isDone = parser.isDone();
        return isDone ? root : null;
    }

    private Node applyS(final Parser parser) {
        final Node nodeA = applyA(parser);
        final Token tokenNext = parser.peekNextToken();
        final String wordNext = Token.getWord(tokenNext);
        final Node nodeS;
        if (tokenNext == null) {
            nodeS = nodeA;
        } else if (AND.equalsIgnoreCase(wordNext)) {
            parser.getNextToken();
            nodeS = new BinaryOperator(AND, nodeA, applyS(parser));
        } else if (OR.equalsIgnoreCase(wordNext)) {
            parser.getNextToken();
            nodeS = new BinaryOperator(OR, nodeA, applyS(parser));
        } else {
            nodeS = nodeA;
        }
        return nodeS;
    }

    private Node applyA(final Parser parser) {
        final Token token = parser.getNextToken();
        final Token tokenNext = parser.peekNextToken();
        final Node nodeA;
        if (token == null) {
            nodeA = null;
        } else if (NOT.equalsIgnoreCase(token.getValue())) {
            nodeA = new UnaryOperator(NOT, applyA(parser));
        } else if (Token.isFunction(token, tokenNext)) {
            nodeA = applyF(token.getValue(), parser);
        } else if (Token.Const.OPEN_PAREN.equals(token.getValue())) {
            nodeA = applyParen(parser);
        } else if (Token.isWord(token)) {
            nodeA = new Operand(token.getValue());
        } else if (Token.isString(token)) {
            nodeA = new Operand(token.getValue());
        } else {
            throw new IllegalArgumentException(token.getValue());
        }
        return nodeA;
    }

    private Node applyF(final String fnName, final Parser parser) {
        final List<Node> args = new ArrayList<>();
        parser.getNextToken();  // open parenthesis
        Token token = parser.peekNextToken();
        while (!Token.Const.CLOSE_PAREN.equals(token.getValue())) {
            args.add(applyS(parser));
            token = parser.peekNextToken();
        }
        parser.getNextToken();  // close parenthesis
        return new MultiOperator(fnName, args);
    }

    private Node applyParen(final Parser parser) {
        final Node nodeS = applyS(parser);
        final Token tokenClose = parser.getNextToken();
        final Node node;
        if (Token.Const.CLOSE_PAREN.equals(tokenClose.getValue())) {
            node = nodeS;
        } else {
            throw new IllegalArgumentException(tokenClose.getValue());
        }
        return node;
    }
}
