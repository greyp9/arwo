package io.github.greyp9.arwo.core.result.type.rowset;

import io.github.greyp9.arwo.core.result.type.Result;
import io.github.greyp9.arwo.core.table.row.RowSet;

public class RowSetResult extends Result {
    private final RowSet rowSet;

    public RowSet getRowSet() {
        return rowSet;
    }

    public RowSetResult(String id, String type, RowSet rowSet) {
        super(id, type);
        this.rowSet = rowSet;
    }
}
