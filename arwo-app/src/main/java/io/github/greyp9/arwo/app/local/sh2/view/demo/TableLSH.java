package io.github.greyp9.arwo.app.local.sh2.view.demo;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.sh2.core.CommandLSH;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.util.Collection;

public final class TableLSH {
    private final Collection<CommandLSH> commands;
    private final AppUserState userState;

    public TableLSH(final Collection<CommandLSH> commands, final AppUserState userState) {
        this.commands = commands;
        this.userState = userState;
    }

    public void addTable(final Element html) throws IOException {
        final RowSetMetaData metaData = createMetaData();
        final RowSet rowSet = createRowSet(metaData);
        final Bundle bundle = userState.getBundle();
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
        final TableContext tableContext = new TableContext(
                viewState, filter, userState.getSubmitID(), App.CSS.TABLE, bundle, userState.getLocus());
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);

    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData("dateSubmit", Types.TIMESTAMP, true),  // i18n metadata
                new ColumnMetaData("in", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("out", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("dateFinish", Types.TIMESTAMP),  // i18n metadata
        };
        return new RowSetMetaData("lsh2HistoryType", columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData) {
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final CommandLSH commandSH : commands) {
            createRow(rowSet, commandSH);
        }
        return rowSet;
    }

    private void createRow(final RowSet rowSet, final CommandLSH command) {
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(command.getDate());
        insertRow.setNextColumn(command.getIn());
        insertRow.setNextColumn(command.getOut());
        insertRow.setNextColumn(command.getDateFinish());
        rowSet.add(insertRow.getRow());
    }
}
