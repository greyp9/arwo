package io.github.greyp9.arwo.core.table.filter;

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

    public final boolean toLocalize() {
        return (!listFilterNew.isEmpty());
    }
}
