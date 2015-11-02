package io.github.greyp9.arwo.core.table.row;

public class Row {
    private final int ordinal;
    private final boolean highlight;
    private final Object[] columns;

    public Row(final int ordinal, final int columnCount) {
        this(ordinal, false, columnCount);
    }

    public Row(final int ordinal, final boolean highlight, final int columnCount) {
        this.ordinal = ordinal;
        this.highlight = highlight;
        this.columns = new Object[columnCount];
    }

    public Row(final int ordinal, final Row row) {
        this.ordinal = ordinal;
        this.highlight = row.highlight;
        this.columns = row.columns;
    }

    public final int getOrdinal() {
        return ordinal;
    }

    public final boolean isHighlight() {
        return highlight;
    }

    public final Object getColumn(final int i) {
        return (inBounds(i)) ? columns[i] : null;
    }

    public final void setColumn(final int i, final Object value) {
        if (inBounds(i)) {
            columns[i] = value;
        }
    }

    private boolean inBounds(final int i) {
        return ((i >= 0) && (i < columns.length));
    }

    public final String getString(final int i) {
        return (String) getColumn(i);
    }

    public final Integer getInteger(final int i) {
        return (Integer) getColumn(i);
    }

    public final Long getLong(final int i) {
        return (Long) getColumn(i);
    }

    public final java.util.Date getDate(final int i) {
        return (java.util.Date) getColumn(i);
    }
}
