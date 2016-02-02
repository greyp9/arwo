package io.github.greyp9.arwo.app.cron.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cron.exec.CronTabExecutor;
import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.cron.service.CronService;
import io.github.greyp9.arwo.core.cron.tab.CronTab;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.cell.Duration;
import io.github.greyp9.arwo.core.table.cell.TableViewButton;
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
import java.security.Principal;
import java.sql.Types;
import java.util.Collection;
import java.util.Date;

@SuppressWarnings("PMD.ExcessiveImports")
public class CronActiveView {
    private final CronService cronService;
    private final AppRequest request;
    private final AppUserState userState;

    public CronActiveView(final CronService cronService, final AppRequest request, final AppUserState userState) {
        this.cronService = cronService;
        this.request = request;
        this.userState = userState;
    }

    public final void addContent(final Element html, final boolean displayOnEmpty) throws IOException {
        final RowSetMetaData metaData = createMetaData();
        final RowSet rowSet = createRowSetComposite(metaData, userState.getPrincipal());
        if ((displayOnEmpty) || (rowSet.getRows() > 0)) {
            final Bundle bundle = request.getBundle();
            final Locus locus = request.getLocus();
            final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
            final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
            TableU.addFooterStandard(table, bundle);
            final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
            final TableContext tableContext = new TableContext(
                    viewState, filter, request.getSubmitID(), App.CSS.TABLE, bundle, locus);
            final TableView tableView = new TableView(table, tableContext);
            tableView.addContentTo(html);
        }
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData("tz", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("tabName", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("jobName", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("line", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("next", Types.TIMESTAMP),  // i18n metadata
                new ColumnMetaData("until", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("now", Types.VARCHAR),  // i18n metadata
        };
        return new RowSetMetaData("cronEntriesType", columns);  // i18n metadata
    }

    private RowSet createRowSetComposite(final RowSetMetaData metaData, final Principal principal) throws IOException {
        final RowSet rowSetComposite = new RowSet(metaData, null, null);
        final Collection<CronTabExecutor> executors = cronService.getExecutors();
        for (final CronTabExecutor executor : executors) {
            final boolean isActive = (!executor.isStopped());
            final boolean isMatchPrincipal = executor.getPrincipal().getName().equals(principal.getName());
            if (isActive && isMatchPrincipal) {
                final RowSet rowSet = createRowSet(metaData, executor.getCronTab());
                rowSet.updateOrdinals(rowSetComposite.getRows());
                rowSetComposite.addAll(rowSet);
            }
        }
        return rowSetComposite;
    }

    private RowSet createRowSet(final RowSetMetaData metaData, final CronTab cronTab) throws IOException {
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final CronJob cronJob : cronTab.getJobs()) {
            addRow(rowSet, cronTab, cronJob);
        }
        return rowSet;
    }

    private void addRow(final RowSet rowSet, final CronTab cronTab, final CronJob cronJob) throws IOException {
        final Date dateRequest = request.getHttpRequest().getDate();
        final Date dateNext = cronJob.getDateNext(dateRequest, cronTab.getTZ(), DurationU.Const.ONE_DAY);
        final String until = DurationU.duration(dateRequest, dateNext);
        final boolean due = (!DurationU.Const.ONE_DAY.equals(until));
        final SubmitToken tokenNow = new SubmitToken(
                App.Target.USER_STATE, App.Action.CRON_NOW, cronTab.getName(), cronJob.getName());
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(cronTab.getTZ().getID());
        insertRow.setNextColumn(cronTab.getName());
        insertRow.setNextColumn(cronJob.getName());
        insertRow.setNextColumn(cronJob.getLine());
        insertRow.setNextColumn(due ? dateNext : null);
        insertRow.setNextColumn(due ? new Duration(DurationU.toMillis(until)) : null);
        insertRow.setNextColumn(new TableViewButton(UTF16.PLAY, request.getSubmitID(), tokenNow.toString()));
        rowSet.add(insertRow.getRow());
    }
}
