package io.github.greyp9.arwo.app.task.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.resource.PathU;
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
import io.github.greyp9.arwo.core.task.type.process.ProcessTask;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import io.github.greyp9.arwo.core.xed.action.XedActionStdin;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.util.Arrays;

public class TaskView {
    private final ProcessTask task;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public TaskView(final ServletHttpRequest httpRequest,
                    final AppUserState userState,
                    final ProcessTask task) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.task = task;
    }

    public final void addContent(final Element html) throws IOException {
        ElementU.addElement(html, Html.DIV, Value.join(Html.SPACE, Arrays.asList(task.getCmd())));
        // means to write to stdin
        if (task.isRunning()) {
            new XedActionStdin(userState.getXedFactory(), userState.getLocale()).addPropertyStripTo(
                    html, userState.getSubmitID(), Arrays.asList(App.Action.STDIN, App.Action.SIGNAL));
        }
        final String dateSubmit = DateX.toFilename(task.getDateSubmit());
        final String hrefStdout = PathU.toDir(httpRequest.getBaseURI(),
                task.getName(), dateSubmit, ProcessTask.Const.STREAM_STDOUT);
        final String hrefStderr = PathU.toDir(httpRequest.getBaseURI(),
                task.getName(), dateSubmit, ProcessTask.Const.STREAM_STDERR);
        final Element divStdout = ElementU.addElement(html, Html.DIV);
        ElementU.addElement(divStdout, Html.A, String.format("stdout(%d)", task.getStdout().getLength()),
                NameTypeValuesU.create(Html.HREF, hrefStdout));
        final Element divStderr = ElementU.addElement(html, Html.DIV);
        ElementU.addElement(divStderr, Html.A, String.format("stderr(%d)", task.getStderr().getLength()),
                NameTypeValuesU.create(Html.HREF, hrefStderr));

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
        final RowSetMetaData metaData = createMetaData("task");
        final RowSet rowSet = new RowSet(metaData, null, null);
        addRow(rowSet, Task.Const.FIELD_NAME, task.getName());
        addRow(rowSet, Task.Const.FIELD_DATE_SUBMIT, XsdDateU.toXSDZMillis(task.getDateSubmit()));
        addRow(rowSet, Task.Const.FIELD_DATE_START, XsdDateU.toXSDZMillis(task.getDateStart()));
        addRow(rowSet, Task.Const.FIELD_DATE_FINISH, XsdDateU.toXSDZMillis(task.getDateFinish()));
        addRow(rowSet, ProcessTask.Const.FIELD_EXIT_VALUE, String.valueOf(task.getExitValue()));
        return rowSet;
    }

    private RowSetMetaData createMetaData(final String id) {
        final ColumnMetaData[] columns = {
                new ColumnMetaData("name", Types.VARCHAR, true),
                new ColumnMetaData("value", Types.VARCHAR),
        };
        return new RowSetMetaData(id, columns);
    }

    private void addRow(final RowSet rowSet, final String name, final Object value) {
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(name);
        insertRow.setNextColumn(value);
        rowSet.add(insertRow.getRow());
    }
}
