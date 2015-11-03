package io.github.greyp9.arwo.core.table.model;

import io.github.greyp9.arwo.core.table.filter.Filters;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.sort.Sorts;

public class Table extends RowSet {
    private final String title;
    private final String summary;

    public Table(final RowSet rowSet, final Sorts sorts, final Filters filters,
                 final String title, final String summary) {
        super(rowSet, sorts, filters);
        this.title = title;
        this.summary = summary;
    }

    public final String getTitle() {
        return title;
    }

    public final String getSummary() {
        return summary;
    }

    public static class Const {
        public static final String TABLE = "table";  // i18n lookup

        public static final String FOOTER_C = "FOOTER_C";
        public static final String FOOTER_L = "FOOTER_L";
        public static final String FOOTER_R = "FOOTER_R";

        public static final String FOOTER_HREF_C = "FOOTER_HREF_C";
        public static final String FOOTER_HREF_L = "FOOTER_HREF_L";
        public static final String FOOTER_HREF_R = "FOOTER_HREF_R";

        public static final String FOOTER_SIZE = "table.footer.size";
    }
}
