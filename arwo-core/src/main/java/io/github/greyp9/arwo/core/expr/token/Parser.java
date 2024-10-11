package io.github.greyp9.arwo.core.expr.token;

public final class Parser {
    private final String expression;
    private int cursor;

    public Parser(final String expression) {
        this.expression = expression.trim();
        this.cursor = 0;
    }

    public boolean isDone() {
        return (cursor == expression.length());
    }

    @SuppressWarnings("WeakerAccess")
    public Token peekNextToken() {
        int cursorPeek = cursor;
        Token token = getNextToken();
        cursor = cursorPeek;
        return token;
    }

    public Token getNextToken() {
        int length = expression.length();
        while ((cursor < length) && (Character.isWhitespace(expression.charAt(cursor)))) {
            ++cursor;
        }
        return (cursor == length) ? null : getNextTokenInner();
    }

    private Token getNextTokenInner() {
        Token token;
        final char c = expression.charAt(cursor);
        if ('(' == c) {
            token = new Token(expression.substring(cursor, cursor + 1), TokenType.CHAR_TOKEN);
            cursor += token.getValue().length();
        } else if (')' == c) {
            token = new Token(expression.substring(cursor, cursor + 1), TokenType.CHAR_TOKEN);
            cursor += token.getValue().length();
        } else if ('\'' == c) {
            ++cursor;
            token = getNextTokenDelimited('\'');
            ++cursor;
        } else if ('\"' == c) {
            ++cursor;
            token = getNextTokenDelimited('\"');
            ++cursor;
        } else {
            token = getNextTokenWord();
        }
        return token;
    }

    private Token getNextTokenDelimited(final char c) {
        int cursorStart = cursor;
        while (expression.charAt(cursor) != c) {
            ++cursor;
            if (cursor == expression.length()) {
                throw new IllegalArgumentException("" + c);
            }
        }
        return new Token(expression.substring(cursorStart, cursor), TokenType.STRING_TOKEN);
    }

    private Token getNextTokenWord() {
        int cursorStart = cursor;
        while (Character.isLetterOrDigit(expression.charAt(cursor))) {
            ++cursor;
            if (cursor == expression.length()) {
                break;
            }
        }
        return new Token(expression.substring(cursorStart, cursor), TokenType.WORD_TOKEN);
    }
}
