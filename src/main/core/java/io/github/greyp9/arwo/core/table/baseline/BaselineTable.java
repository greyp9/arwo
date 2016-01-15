package io.github.greyp9.arwo.core.table.baseline;

import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.table.compare.CellComparator;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.value.Value;

import java.sql.Types;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

public class BaselineTable {
    private final RowSet rowSetNew;
    private final RowSet rowSetOld;
    private final boolean baselineOnLeft;
    private final CellComparator cellComparator;

    public BaselineTable(final RowSet rowSetNew, final RowSet rowSetOld) {
        this.rowSetNew = rowSetNew;
        this.rowSetOld = rowSetOld;
        this.baselineOnLeft = true;
        this.cellComparator = new CellComparator();
    }

    public final RowSet merge(final String identityColumn) {
        // index table rows on identity column
        final Map<Object, Row> rowsNew = index(rowSetNew, identityColumn);
        final Map<Object, Row> rowsOld = index(rowSetOld, identityColumn);
        // enumerate rows to insert
        final Set<Object> keys = new TreeSet<Object>(rowsNew.keySet());
        keys.addAll(rowsOld.keySet());
        // insert merge of each row into table
        final RowSetMetaData metaData = createMetaDataBaseline(rowSetNew.getMetaData(), baselineOnLeft);
        final RowSet rowsMerge = new RowSet(metaData, null, null);
        for (final Object key : keys) {
            rowsMerge.add(merge(metaData, rowsMerge.getRows(), rowsNew.get(key), rowsOld.get(key)));
        }
        rowsMerge.getProperties().putAll(rowSetNew.getProperties());
        return new RowSet(rowsMerge, rowSetNew.getSorts(), rowSetNew.getFilters());
    }

    @SuppressWarnings("PMD.NPathComplexity")
    private Row merge(final RowSetMetaData metaData, final int ordinal, final Row rowNew, final Row rowOld) {
        final int columnCount = metaData.size();
        final boolean isHighlight = ((rowNew != null)) && (rowNew.isHighlight());
        final Row rowMerge = new Row(ordinal, isHighlight, columnCount);
        boolean rowChange = false;
        for (int iOld = 0; (iOld < (columnCount - 1)); ++iOld) {
            final int iNew = (baselineOnLeft ? (iOld + 1) : iOld);
            final Object valueNew = ((rowNew == null) ? null : rowNew.getColumn(iOld));
            final Object valueOld = ((rowOld == null) ? null : rowOld.getColumn(iOld));
            final int compare = cellComparator.compare(valueOld, valueNew);
            if (rowOld == null) {
                rowMerge.setColumn(iNew, valueNew);
            } else if (rowNew == null) {
                rowMerge.setColumn(iNew, valueOld);
            } else if (compare == 0) {
                rowMerge.setColumn(iNew, valueNew);
            } else {
                rowMerge.setColumn(iNew, newBaselineValue(valueNew, valueOld));
                rowChange = true;
            }
        }
        merge(columnCount, rowMerge, rowChange, rowNew, rowOld);
        return rowMerge;
    }

    private Object newBaselineValue(final Object valueNew, final Object valueOld) {
        return new BaselineValue(valueNew, valueOld);
    }

    private void merge(final int columnCount, final Row rowMerge,
                       final boolean rowChange, final Row rowNew, final Row rowOld) {
        // add row baseline cell with value
        final int baselineColumn = (baselineOnLeft ? 0 : (columnCount - 1));
        if (rowOld == null) {
            rowMerge.setColumn(baselineColumn, new BaselineValue(UTF16.CREATE, null));
        } else if (rowNew == null) {
            rowMerge.setColumn(baselineColumn, new BaselineValue(UTF16.DELETE, null));
        } else if (rowChange) {
            rowMerge.setColumn(baselineColumn, new BaselineValue(UTF16.UPDATE, null));
        }
    }

    private static RowSetMetaData createMetaDataBaseline(final RowSetMetaData metaData, final boolean baselineOnLeft) {
        final int columns = metaData.size();
        final ColumnMetaData[] columnsBaseline = new ColumnMetaData[columns + 1];
        for (int iOld = 0; (iOld < columns); ++iOld) {
            final int iNew = (baselineOnLeft ? (iOld + 1) : iOld);
            columnsBaseline[iNew] = metaData.getColumnMetaData(iOld);
        }
        final int baselineColumn = (baselineOnLeft ? 0 : columns);
        columnsBaseline[baselineColumn] = new ColumnMetaData(ViewState.Toggle.BASELINE, Types.VARCHAR);
        return new RowSetMetaData(metaData.getID(), columnsBaseline);
    }

    @SuppressWarnings("PMD.UseConcurrentHashMap")
    private static Map<Object, Row> index(final RowSet rowSet, final String identityColumn) {
        final Map<Object, Row> rowsIndexed = new TreeMap<Object, Row>();
        final RowSetMetaData metaData = rowSet.getMetaData();
        final int identityOrdinal = metaData.getIndex(identityColumn);
        final Iterator<Row> iterator = rowSet.iterator();
        while (iterator.hasNext()) {
            final Row row = iterator.next();
            final Object identity = row.getColumn(identityOrdinal);
            rowsIndexed.put(Value.defaultOnNull(identity, ""), row);
        }
        return rowsIndexed;
    }
}
