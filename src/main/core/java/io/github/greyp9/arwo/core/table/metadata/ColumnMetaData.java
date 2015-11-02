package io.github.greyp9.arwo.core.table.metadata;

import java.io.Serializable;

public class ColumnMetaData implements Serializable {
    private static final long serialVersionUID = -4653612321080777108L;

    private final String name;
    private final String label;
    private final int type;
    private final boolean identity;

    public final String getName() {
        return name;
    }

    public final String getLabel() {
        return ((label == null) ? name : label);
    }

    public final int getType() {
        return type;
    }

    public final boolean isIdentity() {
        return identity;
    }

    public ColumnMetaData(final String name, final int type) {
        this(name, type, false);
    }

    public ColumnMetaData(final String name, final int type, final boolean identity) {
        this(name, null, type, identity);
    }

    public ColumnMetaData(final String name, final String label, final int type, final boolean identity) {
        this.name = name;
        this.label = label;
        this.type = type;
        this.identity = identity;
    }
}
