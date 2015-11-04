package io.github.greyp9.arwo.core.table.state;

import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.table.filter.Filters;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.sort.Sorts;

import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;

public class ViewState {
    private final String name;
    private final Collection<String> hiddenColumns;
    private final Sorts sorts;
    private final Filters filters;
    private final Map<String, RowSet> baselines;

    private boolean connected;
    private boolean openTH;
    private boolean activeBaseline;
    private String filterColumn;
    private Page page;

    public ViewState(final String name) {
        this.name = name;
        this.hiddenColumns = new TreeSet<String>();
        this.sorts = new Sorts();
        this.filters = new Filters();
        this.baselines = new TreeMap<String, RowSet>();

        this.connected = true;
        this.openTH = false;
        this.activeBaseline = false;
        this.filterColumn = null;
        this.page = null;
    }

    public final String getName() {
        return name;
    }

    public final Collection<String> getHiddenColumns() {
        return hiddenColumns;
    }

    public final Sorts getSorts() {
        return sorts;
    }

    public final Filters getFilters() {
        return filters;
    }

    @SuppressWarnings("unused")
    public final Map<String, RowSet> getBaselines() {
        return baselines;
    }

    public final boolean isConnected() {
        return connected;
    }

    public final void setIsConnected(final boolean b) {
        this.connected = b;
    }

    public final boolean isOpenTH() {
        return openTH;
    }

    public final void setIsOpenTH(final boolean b) {
        this.openTH = b;
    }

    public final boolean isActiveBaseline() {
        return activeBaseline;
    }

    public final void setIsActiveBaseline(final boolean b) {
        this.activeBaseline = b;
    }

    public final Page getPage() {
        return page;
    }

    public final void setPage(final Page page) {
        this.page = page;
    }

/*
    public final String getFilterColumn() {
        return filterColumn;
    }

    public String getFilterColumnLocus(Locus locus) {
        return ((filterColumn == null) ? null :
                new Bundle(locus.getBundle()).getString(TableU.getKey(name, filterColumn)));
    }
*/

    public final void setFilterColumn(final String filterColumn) {
        final boolean alreadySet = filterColumn.equals(this.filterColumn);
        this.filterColumn = (alreadySet ? null : filterColumn);
    }

    public final void reset() {
        hiddenColumns.clear();
        sorts.clear();
        filters.clear();
        baselines.clear();

        connected = true;
        openTH = false;
        activeBaseline = false;
        filterColumn = null;
        page = null;
    }


/*
    public final void addBaseline(final String path, final Table table) {
        if (activeBaseline) {
            activeBaseline = false;
            if (getBaselines().containsKey(path)) {
                getBaselines().remove(path);
            } else {
                getBaselines().put(path, table);
            }
        }
    }
*/

/*
    public void localize(RowSetMetaData metaData, Locus locus) {
        boolean toLocalize = filters.toLocalize();
        if (toLocalize) {
            RowSetMetaData metaDataLocus = new RowSetMetaDataLocus(locus.getBundle()).localize(metaData);
            filters.localize(metaDataLocus, locus.getValueX());
        }
    }
*/

    public static class Action {
        // table actions
        public static final String RESET = "reset";  // reset ViewState for table
        // column actions
        public static final String SORT = "sort";  // add a sort to the table ViewState
        public static final String FILTER = "filter";  // add a filter to the table ViewState (filter form)
        public static final String HIDE = "hide";  // add a hidden column to the table ViewState
    }

    public static class Toggle {
        // table toggles
        public static final String BASELINE = "baseline";  // toggle active/inactive baseline state of table ViewState
        public static final String CONNECT = "connect";  // toggle connected/disconnected state of table ViewState
        public static final String PAGE = "page";  // toggle paging of table ViewState
        public static final String RIBBON = "ribbon";  // toggle display/hide state of table ViewState (header ribbon)
        // column toggles
        public static final String FILTERS = "filters";  // toggle display/hide state of table ViewState filter form
    }

    public static class Nav {
        // table nav
        public static final String FIRST = "first";
        public static final String PREVIOUS = "prev";  // previous page of table results
        public static final String NEXT = "next";  // next page of table results
        public static final String LAST = "last";
    }

    public static class Const {
        public static final String COLUMNS = "columns";
    }
}
