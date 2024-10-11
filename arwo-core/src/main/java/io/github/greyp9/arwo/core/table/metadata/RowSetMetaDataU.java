package io.github.greyp9.arwo.core.table.metadata;

import java.util.ArrayList;
import java.util.Iterator;

public final class RowSetMetaDataU {

    private RowSetMetaDataU() {
    }

    public static RowSetMetaData addLeft(final RowSetMetaData metaData, final ColumnMetaData left) {
        final ArrayList<ColumnMetaData> columns = new ArrayList<ColumnMetaData>();
        columns.add(left);
        for (final Iterator<ColumnMetaData> it = metaData.iterator(); it.hasNext(); it.getClass()) {
            columns.add(it.next());
        }
        return new RowSetMetaData(metaData.getID(), columns.toArray(new ColumnMetaData[columns.size()]));
    }

    public static RowSetMetaData removeColumn(final RowSetMetaData metaData, final String name) {
        final ArrayList<ColumnMetaData> columns = new ArrayList<ColumnMetaData>();
        for (final Iterator<ColumnMetaData> it = metaData.iterator(); it.hasNext(); it.getClass()) {
            final ColumnMetaData columnMetaData = it.next();
            if (!columnMetaData.getName().equals(name)) {
                columns.add(columnMetaData);
            }
        }
        return new RowSetMetaData(metaData.getID(), columns.toArray(new ColumnMetaData[columns.size()]));
    }
}
