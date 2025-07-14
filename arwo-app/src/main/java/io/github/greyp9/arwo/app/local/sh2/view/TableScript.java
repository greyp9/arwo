package io.github.greyp9.arwo.app.local.sh2.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.exec.script.ScriptContext;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.Duration;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
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
import java.util.Date;

public final class TableScript {
    private final Date now;
    private final String baseURI;
    private final Collection<ScriptContext> scripts;
    private final AppUserState userState;

    public TableScript(final Date now,
                       final String baseURI,
                       final Collection<ScriptContext> scripts,
                       final AppUserState userState) {
        this.now = now;
        this.baseURI = baseURI;
        this.scripts = scripts;
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
                new ColumnMetaData(App.Action.SELECT, Types.VARCHAR),
                new ColumnMetaData("context", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("stdin", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("crc", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("dateSubmit", Types.TIMESTAMP, true),  // i18n metadata
                new ColumnMetaData("wait", Types.INTEGER),  // i18n metadata
                new ColumnMetaData("run", Types.INTEGER),  // i18n metadata
                new ColumnMetaData("stdout", Types.INTEGER),  // i18n metadata
                new ColumnMetaData("stderr", Types.INTEGER),  // i18n metadata
                new ColumnMetaData("exit", Types.INTEGER),  // i18n metadata
        };
        return new RowSetMetaData("lsh2HistoryType", columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData) {
        final DateX filenameMilli = DateX.Factory.createFilenameMilli();
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final ScriptContext script : scripts) {
            createRow(rowSet, script, filenameMilli);
        }
        return rowSet;
    }

    private void createRow(final RowSet rowSet, final ScriptContext script, final DateX filenameMilli) {
        final String href = PathU.toDir(baseURI, script.getContext(), filenameMilli.toString(script.getDateSubmit()));
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.GEAR, null, href));
        insertRow.setNextColumn(script.getContext());
        insertRow.setNextColumn(script.getCommand());
        insertRow.setNextColumn(CRCU.crc32String(script.getCommand().trim()));
        insertRow.setNextColumn(script.getDateSubmit());
        insertRow.setNextColumn(Duration.toDuration(DurationU.toDuration(
                script.getDateSubmit(), script.getDateStart(), now)));
        insertRow.setNextColumn(Duration.toDuration(DurationU.toDuration(
                script.getDateStart(), script.getDateFinish(), now)));
        insertRow.setNextColumn(script.getStdout().getLength());
        insertRow.setNextColumn(script.getStderr().getLength());
        insertRow.setNextColumn(script.getExitValue());
        insertRow.setNextColumn(script.getDateFinish());
        rowSet.add(insertRow.getRow());
    }
}
