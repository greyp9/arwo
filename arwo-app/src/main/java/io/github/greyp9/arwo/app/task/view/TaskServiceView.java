package io.github.greyp9.arwo.app.task.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.number.NumberScale;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.Duration;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.task.core.Task;
import io.github.greyp9.arwo.core.task.service.TaskService;
import io.github.greyp9.arwo.core.task.type.process.ProcessTask;
import io.github.greyp9.arwo.core.value.Value;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.util.Arrays;
import java.util.concurrent.Future;

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
        final UserStateTable table = new UserStateTable(
                httpRequest, userState, null, httpRequest.getDate());
        table.toTableView(rowSet).addContentTo(html);
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
                new ColumnMetaData(App.Attr.SELECT, Types.DATALINK),
                new ColumnMetaData("name", Types.VARCHAR, true),
                new ColumnMetaData("dateSubmit", Types.TIMESTAMP, true),
                new ColumnMetaData("dateStart", Types.TIMESTAMP),
                new ColumnMetaData("dateFinish", Types.TIMESTAMP),
                new ColumnMetaData("future", Types.VARCHAR),
                new ColumnMetaData("wait", Types.INTEGER),
                new ColumnMetaData("run", Types.INTEGER),
                new ColumnMetaData("crc", Types.VARCHAR),
                new ColumnMetaData("pid", Types.VARCHAR),
                new ColumnMetaData("stdout", Types.INTEGER),
                new ColumnMetaData("stderr", Types.INTEGER),
                new ColumnMetaData("exitValue", Types.INTEGER),
        };
        return new RowSetMetaData(id, columns);
    }

    private void addRow(final RowSet rowSet, final Task task) {
        final String dateSubmit = DateX.toFilename(task.getDateSubmit());
        final String hrefTask = PathU.toDir(httpRequest.getBaseURI(), task.getName(), dateSubmit);
        final Future<?> future = task.getFuture();
        final String futureText = (future == null) ? null
                : future.isCancelled() ? "CANCELLED" : future.isDone() ? "DONE" : null;
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, App.Action.SELECT, hrefTask));
        insertRow.setNextColumn(task.getName());
        insertRow.setNextColumn(task.getDateSubmit());
        insertRow.setNextColumn(task.getDateStart());
        insertRow.setNextColumn(task.getDateFinish());
        insertRow.setNextColumn(futureText);

        insertRow.setNextColumn(Duration.toDuration(DurationU.toDuration(
                task.getDateSubmit(), task.getDateStart(), httpRequest.getDate())));
        insertRow.setNextColumn(Duration.toDuration(DurationU.toDuration(
                task.getDateStart(), task.getDateFinish(), httpRequest.getDate())));

        final ProcessTask processTask = Value.as(task, ProcessTask.class);
        if (processTask == null) {
            addColumnsTask(insertRow);
        } else {
            addColumnsProcessTask(insertRow, processTask);
        }
        insertRow.setNextColumn(task.getExitValue());
        rowSet.add(insertRow.getRow());
    }

    private void addColumnsTask(final InsertRow insertRow) {
        final int emptyColumns = 4;
        for (int i = 0; (i < emptyColumns); ++i) {
            insertRow.setNextColumn(null);
        }
    }

    private void addColumnsProcessTask(final InsertRow insertRow, final ProcessTask task) {
        final String dateSubmit = DateX.toFilename(task.getDateSubmit());
        final String hrefStdout = PathU.toDir(httpRequest.getBaseURI(),
                task.getName(), dateSubmit, ProcessTask.Const.STREAM_STDOUT);
        final String hrefStderr = PathU.toDir(httpRequest.getBaseURI(),
                task.getName(), dateSubmit, ProcessTask.Const.STREAM_STDERR);
        final int lengthStdout = task.getStdout().getLength();
        final int lengthStderr = task.getStderr().getLength();

        insertRow.setNextColumn(CRCU.crc32String(Arrays.asList(task.getCmd()).toString()));
        insertRow.setNextColumn((task.getPid() == null) ? null : Long.toString(task.getPid()));
        insertRow.setNextColumn(new TableViewLink(NumberScale.toString(lengthStdout), null, PathU.toDir(hrefStdout)));
        insertRow.setNextColumn(new TableViewLink(NumberScale.toString(lengthStderr), null, PathU.toDir(hrefStderr)));
    }
}
