package io.github.greyp9.arwo.core.expr;

public class Grammar {
    private final String expression;

    public Grammar(String expression) {
        this.expression = expression;
    }

    public Tree createTree() {
        final Parser parser = new Parser(expression);
        final Node root = ApplyS(parser);
        final boolean isDone = parser.isDone();
        return isDone ? new Tree(root) : null;
    }

    private Node ApplyS(Parser parser) {
        final Node node = ApplyA(parser);
        final Token token = parser.peekNextToken();
        final boolean isWord = ((token != null) && (TokenType.WORD_TOKEN == token.getType()));
        if (isWord && Token.Const.AND.equalsIgnoreCase(token.getValue())) {
            parser.getNextToken();
            return new Operator(node, ApplyS(parser), Token.Const.AND);
        } else if (isWord && Token.Const.OR.equalsIgnoreCase(token.getValue())) {
            parser.getNextToken();
            return new Operator(node, ApplyS(parser), Token.Const.OR);
        } else {
            return node;
        }
    }

    private Node ApplyA(Parser parser) {
        final Token token = parser.getNextToken();
        final boolean isWord = (TokenType.WORD_TOKEN == token.getType());
        if (isWord && Token.Const.NOT.equalsIgnoreCase(token.getValue())) {
            return new Operator(null, ApplyA(parser), Token.Const.NOT);
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

    private Node applyParen(Parser parser) {
        Node node = ApplyS(parser);
        final Token tokenClose = parser.getNextToken();
        if (Token.Const.CLOSE_PAREN.equals(tokenClose.getValue())) {
            return node;
        } else {
            throw new IllegalArgumentException(tokenClose.getValue());
        }
    }
}
