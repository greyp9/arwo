package io.github.greyp9.arwo.core.table.insert;

import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.row.RowSet;

public class InsertRow {
    private final Row row;
    private int column;

    public InsertRow(final RowSet rowSet) {
        this.row = new Row(rowSet.getRows(), rowSet.getMetaData().size());
        column = -1;
    }

    @SuppressWarnings("PMD.AssignmentInOperand")
    public InsertRow(final RowSet rowSet, final Row rowSource) {
        this.row = new Row(rowSet.getRows(), rowSet.getMetaData().size());
        column = -1;
        final int columnCount = rowSet.getColumns();
        while (++column < columnCount) {
            row.setColumn(column, rowSource.getColumn(column));
        }
    }

    public final void setNextColumn(final Object value) {
        row.setColumn(++column, value);
    }

    public final Row getRow() {
        return row;
    }

    public static InsertRow create(final RowSet rowSet) {
        return new InsertRow(rowSet);
    }

    public static InsertRow create(final RowSet rowSet, final Row rowSource) {
        return new InsertRow(rowSet, rowSource);
    }
}
