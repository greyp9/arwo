package io.github.greyp9.arwo.core.table.row;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.table.compare.RowComparator;
import io.github.greyp9.arwo.core.table.filter.Filter;
import io.github.greyp9.arwo.core.table.filter.Filters;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.sort.Sorts;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.SortedSet;
import java.util.TreeSet;

public class RowSet {
    private final Date date;
    private final RowSetMetaData metaData;
    private final Sorts sorts;
    private final Filters filters;
    private final Properties properties;
    private final RowComparator comparator;
    private final SortedSet<Row> rows;

    public RowSet(final RowSetMetaData metaData, final Sorts sorts, final Filters filters) {
        this.date = new Date();
        this.metaData = metaData;
        this.sorts = ((sorts == null) ? new Sorts() : sorts);
        this.filters = ((filters == null) ? new Filters() : filters);
        this.properties = new Properties();
        this.comparator = new RowComparator(this.metaData, this.sorts);
        this.rows = new TreeSet<Row>(comparator);
    }

    public RowSet(final RowSet rowSet, final Sorts sorts, final Filters filters) {
        this(rowSet.getMetaData(), sorts, filters);
        this.properties.putAll(rowSet.getProperties());
        this.addAll(rowSet);
    }

    public final Date getDate() {
        return DateU.copy(date);
    }

    public final RowSetMetaData getMetaData() {
        return metaData;
    }

    public final String getID() {
        return metaData.getID();
    }

    public final Sorts getSorts() {
        return sorts;
    }

    public final Filters getFilters() {
        return filters;
    }

    public final Properties getProperties() {
        return properties;
    }

    public final int getRows() {
        return rows.size();
    }

    public final int getColumns() {
        return metaData.size();
    }

    public final int getIndex(final String name) {
        return metaData.getIndex(name);
    }

    public final void addAll(final RowSet rowSet) {
        final Iterator<Row> iterator = rowSet.iterator();
        while (iterator.hasNext()) {
            add(iterator.next());
        }
    }

    public final void add(final Row row) {
        add(row, true);
    }

    public final void add(final Row row, final boolean applyFilters) {
        if ((row != null) && ((!applyFilters) || (matchesFilters(row)))) {
            rows.add(row);
        }
    }

    private boolean matchesFilters(final Row row) {
        boolean matchesFilters = true;
        final Iterator<Filter> iterator = filters.iterator();
        while (iterator.hasNext() && matchesFilters) {
            if (!matchesFilter(iterator.next(), row)) {
                matchesFilters = false;
            }
        }
        return matchesFilters;
    }

    private boolean matchesFilter(final Filter filter, final Row row) {
        final int index = metaData.getIndex(filter.getName());
        return ((index < 0) || (filter.matches(comparator.getCellComparator(), row.getColumn(index))));
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public final void updateOrdinals() {
        int ordinal = -1;
        final List<Row> rowsUpdate = new ArrayList<Row>();
        for (final Row row : rows) {
            rowsUpdate.add(new Row(++ordinal, row));
        }
        rows.clear();
        rows.addAll(rowsUpdate);
    }

    public final Iterator<Row> iterator() {
        return rows.iterator();
    }

    public final Object get(final Row row, final String name) {
        return row.getColumn(getIndex(name));
    }

/*
    public Boolean getBoolean(Row row, String name) {
        return (Boolean) get(row, name);
    }

    public Date getDate(Row row, String name) {
        return (Date) get(row, name);
    }

    public Integer getInteger(Row row, String name) {
        return (Integer) get(row, name);
    }

    public Long getLong(Row row, String name) {
        return (Long) get(row, name);
    }

    public String getString(Row row, String name) {
        return (String) get(row, name);
    }
*/
}
