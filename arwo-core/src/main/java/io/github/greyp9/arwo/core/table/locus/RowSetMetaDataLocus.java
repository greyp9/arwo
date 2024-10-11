package io.github.greyp9.arwo.core.table.locus;

import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;

public class RowSetMetaDataLocus {
    private final Bundle bundle;

    public RowSetMetaDataLocus(final Bundle bundle) {
        this.bundle = bundle;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public final RowSetMetaData localize(final RowSetMetaData metaData) {
        final int columns = metaData.size();
        final ColumnMetaData[] columnsLocus = new ColumnMetaData[columns];
        for (int i = 0; (i < columns); ++i) {
            final ColumnMetaData column = metaData.getColumnMetaData(i);
            if (column != null) {
                final String key = TableU.getKey(metaData.getID(), column.getName());
                final String label = bundle.getString(key);
                columnsLocus[i] = new ColumnMetaData(column.getName(), label, column.getType(), column.isIdentity());
            }
        }
        return new RowSetMetaData(metaData.getID(), columnsLocus);
    }
}
