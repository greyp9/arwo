package io.github.greyp9.arwo.core.table.metadata;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Iterator;

public class RowSetMetaData implements Serializable {
    private static final long serialVersionUID = 5395030489971059299L;

    private final String id;
    private final ColumnMetaData[] columns;

    public final String getID() {
        return id;
    }

    public RowSetMetaData(final String id, final ColumnMetaData... columns) {
        this.id = id;
        this.columns = columns;
    }

    private static String getIdentity(final ColumnMetaData... columns) {
        String identity = null;
        for (final ColumnMetaData column : columns) {
            if (column.isIdentity()) {
                identity = column.getName();
            }
        }
        return identity;
    }

    public final int size() {
        return columns.length;
    }

    public final String getIdentity() {
        return getIdentity(columns);
    }

    public final Iterator<ColumnMetaData> iterator() {
        return Arrays.asList(columns).iterator();
    }

    public final String getName(final int i) {
        final ColumnMetaData columnMetaData = getColumnMetaData(i);
        return ((columnMetaData == null) ? null : columnMetaData.getName());
    }

    public final String getLabel(final int i) {
        final ColumnMetaData columnMetaData = getColumnMetaData(i);
        return ((columnMetaData == null) ? null : columnMetaData.getLabel());
    }

    public final String getLabel(final String name) {
        return getLabel(getIndex(name));
    }

    public final int getType(final int i) {
        final ColumnMetaData columnMetaData = getColumnMetaData(i);
        return ((columnMetaData == null) ? 0 : columnMetaData.getType());
    }

    public final int getIndex(final String text) {
        int index = -1;
        for (int i = 0; (i < columns.length); ++i) {
            final boolean isMatchName = text.equals(columns[i].getName());
            final boolean isMatchLabel = text.equals(columns[i].getLabel());
            if (isMatchName || isMatchLabel) {
                index = i;
            }
        }
        return index;
    }

    public final ColumnMetaData getColumnMetaData(final int i) {
        return (inBounds(i) ? columns[i] : null);
    }

    private boolean inBounds(final int i) {
        return ((i >= 0) && (i < columns.length));
    }
}
