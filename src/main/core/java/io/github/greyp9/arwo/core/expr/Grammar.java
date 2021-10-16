package io.github.greyp9.arwo.core.expr;

public final class Grammar {
    private final String expression;

    public Grammar(final String expression) {
        this.expression = expression;
    }

    public Tree createTree() {
        final Parser parser = new Parser(expression);
        final Node root = applyS(parser);
        final boolean isDone = parser.isDone();
        return isDone ? new Tree(root) : null;
    }

    private Node applyS(final Parser parser) {
        final Node node = applyA(parser);
        final Token token = parser.peekNextToken();
        final boolean isWord = ((token != null) && (TokenType.WORD_TOKEN == token.getType()));
        if (isWord && Token.Const.AND.equalsIgnoreCase(token.getValue())) {
            parser.getNextToken();
            return new Operator(node, applyS(parser), Token.Const.AND);
        } else if (isWord && Token.Const.OR.equalsIgnoreCase(token.getValue())) {
            parser.getNextToken();
            return new Operator(node, applyS(parser), Token.Const.OR);
        } else {
            return node;
        }
    }

    private Node applyA(final Parser parser) {
        final Token token = parser.getNextToken();
        final boolean isWord = (TokenType.WORD_TOKEN == token.getType());
        if (isWord && Token.Const.NOT.equalsIgnoreCase(token.getValue())) {
            return new Operator(null, applyA(parser), Token.Const.NOT);
        } else if (Token.Const.OPEN_PAREN.equals(token.getValue())) {
            return applyParen(parser);
        } else if (isWord) {
            return new Operand(token.getValue());
        } else if (token.getType() == TokenType.STRING_TOKEN) {
            return new Operand(token.getValue());
        } else {
            throw new IllegalArgumentException(token.getValue());
        }
    }

    private Node applyParen(final Parser parser) {
        Node node = applyS(parser);
        final Token tokenClose = parser.getNextToken();
        if (Token.Const.CLOSE_PAREN.equals(tokenClose.getValue())) {
            return node;
        } else {
            throw new IllegalArgumentException(tokenClose.getValue());
        }
    }
}
