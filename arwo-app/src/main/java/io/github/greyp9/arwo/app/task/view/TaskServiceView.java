package io.github.greyp9.arwo.app.task.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.number.NumberScale;
import io.github.greyp9.arwo.core.resource.PathU;
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
import io.github.greyp9.arwo.core.task.core.Task;
import io.github.greyp9.arwo.core.task.service.TaskService;
import io.github.greyp9.arwo.core.task.type.process.ProcessTask;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;

public class TaskServiceView {
    private final TaskService taskService;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public TaskServiceView(final ServletHttpRequest httpRequest,
                           final AppUserState userState,
                           final TaskService taskService) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.taskService = taskService;
    }

    public final void addContent(final Element html) throws IOException {
        final RowSet rowSet = createRowSet();
        final Bundle bundle = userState.getBundle();
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(
                httpRequest.getBaseURI(), rowSet.getMetaData(), bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
        final TableContext tableContext = new TableContext(
                viewState, filter, userState.getSubmitID(), App.CSS.TABLE, bundle, locus);
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
    }

    private RowSet createRowSet() {
        final RowSetMetaData metaData = createMetaData(taskService.getName());
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (Task task : taskService.getTasks()) {
            addRow(rowSet, task);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData(final String id) {
        final ColumnMetaData[] columns = {
                new ColumnMetaData("name", Types.VARCHAR, true),
                new ColumnMetaData("dateInvoke", Types.TIMESTAMP, true),
                new ColumnMetaData("dateStart", Types.TIMESTAMP),
                new ColumnMetaData("dateFinish", Types.TIMESTAMP),
                new ColumnMetaData("stdout", Types.INTEGER),
                new ColumnMetaData("stderr", Types.INTEGER),
        };
        return new RowSetMetaData(id, columns);
    }

    private void addRow(final RowSet rowSet, final Task task) {
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(task.getName());
        insertRow.setNextColumn(task.getDateInvoke());
        insertRow.setNextColumn(task.getDateStart());
        insertRow.setNextColumn(task.getDateFinish());
        Value.asOptional(task, ProcessTask.class).ifPresent(pt -> addColumnsProcessTask(insertRow, pt));
        rowSet.add(insertRow.getRow());
    }

    private void addColumnsProcessTask(final InsertRow insertRow, final ProcessTask task) {
        final String dateInvoke = DateX.toFilename(task.getDateInvoke());
        final String hrefStdout = PathU.toDir(httpRequest.getBaseURI(), task.getName(), dateInvoke, "stdout");
        final String hrefStderr = PathU.toDir(httpRequest.getBaseURI(), task.getName(), dateInvoke, "stderr");
        final int lengthStdout = task.getStdout().getLength();
        final int lengthStderr = task.getStderr().getLength();
        insertRow.setNextColumn(new TableViewLink(NumberScale.toString(lengthStdout), null, PathU.toDir(hrefStdout)));
        insertRow.setNextColumn(new TableViewLink(NumberScale.toString(lengthStderr), null, PathU.toDir(hrefStderr)));
    }
}
