package io.github.greyp9.arwo.core.result.type.rowset;

import io.github.greyp9.arwo.core.result.type.Result;
import io.github.greyp9.arwo.core.table.row.RowSet;

public class RowSetResult extends Result {
    private final RowSet rowSet;

    public final RowSet getRowSet() {
        return rowSet;
    }

    public RowSetResult(final String id, final String type, final RowSet rowSet) {
        super(id, type);
        this.rowSet = rowSet;
    }
}
