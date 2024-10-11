package io.github.greyp9.arwo.core.table.compare;

import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.sort.Sort;
import io.github.greyp9.arwo.core.table.sort.Sorts;

import java.io.Serializable;
import java.util.Iterator;

public class RowComparator implements Serializable, java.util.Comparator<Row> {
    private static final long serialVersionUID = -4802641965967856746L;

    private final RowSetMetaData metaData;
    private final Sorts sorts;
    private final CellComparator cellComparator;

    public final CellComparator getCellComparator() {
        return cellComparator;
    }

    public RowComparator(final RowSetMetaData metaData, final Sorts sorts) {
        this.metaData = metaData;
        this.sorts = sorts;
        this.cellComparator = new CellComparator();
    }

    @Override
    public final int compare(final Row left, final Row right) {
        int compare = 0;
        final Iterator<Sort> iterator = sorts.iterator();
        while (iterator.hasNext()) {
            final Sort sort = iterator.next();
            final int index = metaData.getIndex(sort.getName());
            compare = compareColumn(index, left, right);
            if (!sort.isAscending()) {
                compare *= -1;
            }
            if (compare != 0) {
                break;
            }
        }
        return ((compare == 0) ? (left.getOrdinal() - right.getOrdinal()) : compare);
    }

    private int compareColumn(final int index, final Row left, final Row right) {
        return cellComparator.compare(left.getColumn(index), right.getColumn(index));
    }
}
