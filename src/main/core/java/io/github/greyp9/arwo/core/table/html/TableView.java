package io.github.greyp9.arwo.core.table.html;

import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.baseline.BaselineTable;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import org.w3c.dom.Element;

import java.io.IOException;

public class TableView {
    private final Table table;
    private final TableContext context;
    private final ViewState viewState;

    public TableView(final Table table, final TableContext context) {
        this.table = table;
        this.context = context;
        this.viewState = context.getViewState();
    }

    public final void addContentTo(final Element html) throws IOException {
        final String baselineID = ((table.getTitle() == null) ? table.getID() : table.getTitle());
        // update baseline (if needed)
        viewState.addBaseline(baselineID, table);
        // display normal table, or baseline table (if baseline and table identity column exist)
        final boolean baselineSet = viewState.getBaselines().containsKey(baselineID);
        final String identity = table.getMetaData().getIdentity();
        if (baselineSet && (identity != null)) {
            final RowSet rowSetBaseline = viewState.getBaselines().get(baselineID);
            final BaselineTable baselineTable = new BaselineTable(table, rowSetBaseline);
            final RowSet rowSetMerge = baselineTable.merge(identity);
            final Table tableMerge = new Table(rowSetMerge, table.getSorts(), table.getFilters(),
                    table.getTitle(), table.getSummary());
            final Locus locus = context.getLocus();
            final String baselineDate = locus.toString(rowSetBaseline.getDate());
            //final String baselineDuration = DurationU.duration(rowSetBaseline.getDate(), table.getDate());
            //final String baselineText = Value.join(" ", baselineDate, baselineDuration);
            tableMerge.getProperties().setProperty(Table.Const.FOOTER_R, baselineDate);
            final TableViewInner tableView = new TableViewInner(tableMerge, context);
            tableView.addContentTo(html);
        } else {
            final TableViewInner tableView = new TableViewInner(table, context);
            tableView.addContentTo(html);
        }
    }
}
