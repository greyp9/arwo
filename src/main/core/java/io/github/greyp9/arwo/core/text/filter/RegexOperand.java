package io.github.greyp9.arwo.core.text.filter;

import io.github.greyp9.arwo.core.expr.Node;

import java.util.regex.Pattern;

/**
 * A {@link Node} specialization in support of regex queries of text streams.
 */
public final class RegexOperand extends Node {
    private final Pattern value;

    public Pattern getValue() {
        return value;
    }

    public RegexOperand(final String value) {
        this.value = Pattern.compile(value);
    }

    @Override
    public String render() {
        return value.pattern();
    }
}
