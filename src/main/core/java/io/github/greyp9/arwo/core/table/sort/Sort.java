package io.github.greyp9.arwo.core.table.sort;

import java.io.Serializable;

public class Sort implements Serializable {
    private static final long serialVersionUID = 6164794888735621372L;

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
