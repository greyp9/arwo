package io.github.greyp9.arwo.app.core.view.history;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.Duration;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.cell.TableViewLinks;
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
import java.util.Arrays;
import java.util.Collection;

public class AppHistoryView {
    private final String tableID;
    private final boolean includeContext;
    private final Collection<Script> scripts;
    private final Bundle bundle;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public AppHistoryView(final String tableID, final boolean includeContext, final Collection<Script> scripts,
                          final Bundle bundle, final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.tableID = tableID;
        this.includeContext = includeContext;
        this.scripts = scripts;
        this.bundle = bundle;
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse addContentTo(final Element html) throws IOException {
        final RowSetMetaData metaData = createMetaData(tableID);
        final RowSet rowSet = createRowSet(metaData);
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
        final TableContext tableContext = new TableContext(
                viewState, filter, userState.getSubmitID(), App.CSS.TABLE, bundle, userState.getLocus());
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
        return null;
    }

    private RowSetMetaData createMetaData(final String id) {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(App.Action.SELECT, Types.VARCHAR),
                new ColumnMetaData("context", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("stdin", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("crc", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("dateSubmit", Types.TIMESTAMP, true),  // i18n metadata
                new ColumnMetaData("state", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("wait", Types.INTEGER),  // i18n metadata
                new ColumnMetaData("run", Types.INTEGER),  // i18n metadata
                new ColumnMetaData("stdout", Types.INTEGER),  // i18n metadata
                new ColumnMetaData("stderr", Types.INTEGER),  // i18n metadata
                new ColumnMetaData("exit", Types.INTEGER),  // i18n metadata
        };
        return new RowSetMetaData(id, columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData) {
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final Script script : scripts) {
            createRow(rowSet, script);
        }
        return rowSet;
    }

    private void createRow(final RowSet rowSet, final Script script) {
        final boolean isStarted = (script.getStart() != null);
        final boolean isFinished = (script.getFinish() != null);
        final boolean isProgress = script.isProgress();
        final String id = script.getID();
        final String href = includeContext
                ? PathU.toDir(httpRequest.getBaseURI(), script.getContext(), id)
                : PathU.toDir(httpRequest.getBaseURI(), id);
        final String hrefResults = href + "?results";
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLinks(Arrays.asList(
                new TableViewLink(UTF16.GEAR, null, href),
                new TableViewLink(UTF16.CHECK, null, isFinished ? hrefResults : null))));
        insertRow.setNextColumn(script.getContext());
        insertRow.setNextColumn(script.getText());
        insertRow.setNextColumn(CRCU.crc32String(script.getText().trim()));
        insertRow.setNextColumn(script.getDate());
        insertRow.setNextColumn(getState(isStarted, isFinished));
        insertRow.setNextColumn(Duration.toDuration(script.getDelay(httpRequest.getDate())));
        insertRow.setNextColumn(Duration.toDuration(script.getElapsed(httpRequest.getDate())));
        insertRow.setNextColumn(isProgress ? script.getStdoutLength() : null);
        insertRow.setNextColumn(isProgress ? script.getStderrLength() : null);
        insertRow.setNextColumn(script.getExitValue());
        rowSet.add(insertRow.getRow());
    }

    private String getState(final boolean isStarted, final boolean isFinished) {
        final String queued = bundle.getString("AppHistoryView.QUEUED");
        final String started = bundle.getString("AppHistoryView.STARTED");
        final String finished = bundle.getString("AppHistoryView.FINISHED");
        return (isFinished ? finished : (isStarted ? started : queued));
    }
}
