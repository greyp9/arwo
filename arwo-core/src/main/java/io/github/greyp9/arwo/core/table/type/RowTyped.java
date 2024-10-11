package io.github.greyp9.arwo.core.table.type;

import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.Row;

public class RowTyped {
    private final RowSetMetaData metaData;
    private final Row row;

    public RowTyped(final RowSetMetaData metaData, final Row row) {
        this.metaData = metaData;
        this.row = row;
    }

    public final void update(final String name, final Object value) {
        row.setColumn(metaData.getIndex(name), value);
    }
}
