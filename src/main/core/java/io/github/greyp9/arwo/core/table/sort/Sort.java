package io.github.greyp9.arwo.core.table.sort;

public class Sort {
    private final String name;
    private final boolean ascending;

    public Sort(final String name, final boolean ascending) {
        this.name = name;
        this.ascending = ascending;
    }

    public final String getName() {
        return name;
    }

    public final boolean isAscending() {
        return ascending;
    }
}
