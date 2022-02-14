package io.github.greyp9.arwo.core.expr.token;

public final class Token {
    private final String value;
    private final TokenType type;

    public String getValue() {
        return value;
    }

    public TokenType getType() {
        return type;
    }

    public Token(final String value, final TokenType type) {
        this.value = value;
        this.type = type;
    }

    public static boolean isWord(final Token token) {
        return (token != null) && (token.type.equals(TokenType.WORD_TOKEN));
    }

    public static String getWord(final Token token) {
        return isWord(token) ? token.getValue() : null;
    }

    public static boolean isString(final Token token) {
        return (token != null) && (token.type.equals(TokenType.STRING_TOKEN));
    }

    public static boolean isFunction(final Token token, final Token token2) {
        final boolean isFnName = (token != null) && (token.type.equals(TokenType.WORD_TOKEN));
        final boolean isOpenParen = (token2 != null) && (Const.OPEN_PAREN.equals(token2.getValue()));
        return isFnName && isOpenParen;
    }

    public static class Const {
        public static final String AND = "AND";
        public static final String OR = "OR";
        public static final String NOT = "NOT";

        public static final String OPEN_PAREN = "(";
        public static final String CLOSE_PAREN = ")";
    }
}
