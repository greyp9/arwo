package io.github.greyp9.arwo.core.env;

/**
 * Container for a single piece of information about the process environment.
 */
public final class EnvironmentAtom {
    private final int ordinal;
    private final String key;
    private final String value;

    public int getOrdinal() {
        return ordinal;
    }

    public String getKey() {
        return key;
    }

    public String getValue() {
        return value;
    }

    public EnvironmentAtom(final int ordinal, final String key, final String value) {
        this.ordinal = ordinal;
        this.key = key;
        this.value = value;
    }
}
