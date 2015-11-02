package io.github.greyp9.arwo.core.table.sort;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

public class Sorts implements Serializable {
    private static final long serialVersionUID = 5933851633623004165L;

    private final List<Sort> listSort;

    public Sorts(final Sort... sorts) {
        this.listSort = new ArrayList<Sort>();
        Collections.addAll(this.listSort, sorts);
        normalize();
    }

    public final void clear() {
        listSort.clear();
    }

    public final void add(final String name) {
        Sort sortOld = null;
        for (final Sort sort : listSort) {
            if (sort.getName().equals(name)) {
                sortOld = sort;
                break;
            }
        }
        if (sortOld == null) {
            listSort.add(0, new Sort(name, true));
        } else if (sortOld.isAscending()) {
            listSort.remove(sortOld);
            listSort.add(0, new Sort(name, false));
        } else {
            listSort.remove(sortOld);
        }
        normalize();
    }

    public final Boolean get(final String name) {
        Boolean order = null;
        for (final Sort sort : listSort) {
            if (sort.getName().equals(name)) {
                order = sort.isAscending();
                break;
            }
        }
        return order;
    }

    public final int size() {
        return listSort.size();
    }

    public final Iterator<Sort> iterator() {
        return this.listSort.iterator();
    }

    private void normalize() {
        while (listSort.size() > Const.LIMIT) {
            listSort.remove(Const.LIMIT);
        }
    }

    private static class Const {
        private static final int LIMIT = 3;
    }
}
