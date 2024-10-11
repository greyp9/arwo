package io.github.greyp9.arwo.core.table.cell;

import io.github.greyp9.arwo.core.lang.CompareU;

public class Duration implements Comparable<Duration> {
    private final Long value;

    public Duration(final Long value) {
        this.value = value;
    }

    public final Long getValue() {
        return value;
    }

    @Override
    public final int compareTo(final Duration duration) {
        return CompareU.compare(value, (duration == null) ? null : duration.getValue());
    }

    // findbugs
    @Override
    public final boolean equals(final Object o) {
        return ((o instanceof Duration) && (compareTo((Duration) o) == 0));
    }

    // findbugs
    @Override
    public final int hashCode() {
        return ((value == null) ? 0 : value.hashCode());
    }

    public static Duration toDuration(final Long value) {
        return (value == null) ? null : new Duration(value);
    }
}
