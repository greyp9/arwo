package io.github.greyp9.arwo.core.table.filter;

import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.value.Value;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

public class Filters {
    private final List<Filter> listFilter;
    private final List<Filter> listFilterNew;

    public Filters(final Filter... filters) {
        this.listFilter = new ArrayList<Filter>();
        Collections.addAll(this.listFilter, filters);
        this.listFilterNew = new ArrayList<Filter>();
    }

    public final void clear() {
        listFilter.clear();
        listFilterNew.clear();
    }

    public final void add(final Filter filter) {
        listFilterNew.add(filter);
    }

    public final int size() {
        return listFilter.size();
    }

    public final Iterator<Filter> iterator() {
        return listFilter.iterator();
    }

    public final boolean toNormalize() {
        return (!listFilterNew.isEmpty());
    }

    public final void normalize(final RowSetMetaData metaData, final Locus locus) {
        for (final Filter filterNew : listFilterNew) {
            if (filterNew.getOperator() == null) {
                listFilter.clear();
            } else {
                add(listFilter, normalize(metaData, locus, filterNew));
            }
        }
        listFilterNew.clear();
    }

    private void add(final List<Filter> filters, final Filter filter) {
        if (filter != null) {
            filters.add(filter);
        }
    }

    private Filter normalize(final RowSetMetaData metaData, final Locus locus, final Filter filter) {
        // find native column name for input
        final int columnIndex = metaData.getIndex(filter.getName());
        final String name = metaData.getName(columnIndex);
        // find native column value for input
        final String value = Value.defaultOnEmpty(filter.getValue().toString(), null);
        // convert strings to native column type
        final Object valueTyped = locus.toValue(metaData.getType(columnIndex), value);
        final boolean isFilter = (name != null);
        return (isFilter ? new Filter(columnIndex, name, filter.getOperator(), valueTyped) : null);
    }
}
