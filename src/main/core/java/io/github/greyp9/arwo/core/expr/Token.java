package io.github.greyp9.arwo.core.expr;

public class Token {
    private final String value;
    private final TokenType type;

    public String getValue() {
        return value;
    }

    public TokenType getType() {
        return type;
    }

    public Token(String value, TokenType type) {
        this.value = value;
        this.type = type;
    }

    public static class Const {
        public static final String AND = "AND";
        public static final String OR = "OR";
        public static final String NOT = "NOT";

        public static final String OPEN_PAREN = "(";
        public static final String CLOSE_PAREN = ")";
    }
}
