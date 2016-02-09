package io.github.greyp9.arwo.app.jdbc.sh.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.Interval;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.jdbc.query.History;
import io.github.greyp9.arwo.core.jdbc.query.Query;
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
import java.util.Date;

public class JDBCHistoryView {
    private final String tableID;
    private final History history;
    private final Bundle bundle;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;


    public JDBCHistoryView(final String tableID, final History history,
                           final Bundle bundle, final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.tableID = tableID;
        this.history = history;
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
                new ColumnMetaData("sql", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("dateSubmit", Types.TIMESTAMP, true),  // i18n metadata
                new ColumnMetaData("state", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("wait", Types.INTEGER),  // i18n metadata
                new ColumnMetaData("run", Types.INTEGER),  // i18n metadata
                new ColumnMetaData("exitValue", Types.INTEGER),  // i18n metadata
        };
        return new RowSetMetaData(id, columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData) throws IOException {
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final Query query : history.getHistory()) {
            createRow(rowSet, query);
        }
        return rowSet;
    }

    private void createRow(final RowSet rowSet, final Query query) {
        final Interval interval = query.getResults().getInterval();
        final Date dateStart = interval.getDateStart();
        final Date dateFinish = interval.getDateFinish();
        final boolean isStarted = (dateStart != null);
        final boolean isFinished = (dateFinish != null);
        final Long delay = DurationU.toMillis(DurationU.duration(query.getDate(), dateStart, httpRequest.getDate()));
        final Long elapsed = DurationU.toMillis(DurationU.duration(dateStart, dateFinish, httpRequest.getDate()));
        final String id = query.getID();
        final String href = PathU.toDir(httpRequest.getBaseURI(), query.getContext(), id);
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, null, href));
        insertRow.setNextColumn(query.getContext());
        insertRow.setNextColumn(query.getText());
        insertRow.setNextColumn(query.getDate());
        insertRow.setNextColumn(getState(isStarted, isFinished));
        insertRow.setNextColumn(Duration.toDuration(delay));
        insertRow.setNextColumn(Duration.toDuration(elapsed));
        insertRow.setNextColumn(query.getExitValue());
        rowSet.add(insertRow.getRow());
    }

    private String getState(final boolean isStarted, final boolean isFinished) {
        final String queued = bundle.getString("AppHistoryView.QUEUED");
        final String started = bundle.getString("AppHistoryView.STARTED");
        final String finished = bundle.getString("AppHistoryView.FINISHED");
        return (isFinished ? finished : (isStarted ? started : queued));
    }
}
