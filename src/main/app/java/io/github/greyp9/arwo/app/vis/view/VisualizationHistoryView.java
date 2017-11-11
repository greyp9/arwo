package io.github.greyp9.arwo.app.vis.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.vis.core.VisualizationRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogram;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.sql.Types;
import java.util.Date;

public class VisualizationHistoryView extends VisualizationView {
    private final TimeHistogram histogram;

    public VisualizationHistoryView(final ServletHttpRequest httpRequest, final VisualizationRequest request,
                                    final AppUserState userState, final TimeHistogram histogram) {
        super(httpRequest, request, userState);
        this.histogram = histogram;
    }

    protected HttpResponse addContentTo(final Element html) throws IOException {
        final RowSetMetaData metaData = createMetaData();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final RowSet rowSet = createRowSet(metaData);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        final TableContext tableContext = new TableContext(
                viewState, null, userState.getSubmitID(), App.CSS.TABLE, bundle, locus);
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
        return null;
    }

    protected void addMenuNav(final Element html) {
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData("link", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("date", Types.TIMESTAMP, true),  // i18n metadata
                new ColumnMetaData("size", Types.BIGINT),  // i18n metadata
        };
        return new RowSetMetaData("visHistoryType", columns);  // i18n metadata
    }

    private RowSet createRowSet(final RowSetMetaData metaData) throws IOException {
        final DateX dateX = new DateX(HttpDateU.Const.DEFAULT, DateU.Const.TZ_GMT);
        final File folder = new File(histogram.getFolder());
        final String pattern = String.format("%s.*.xml", histogram.getName());
        final FindInFolderQuery query = new FindInFolderQuery(folder, pattern, false);
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final File file : query.getFound()) {
            addRow(rowSet, file, dateX);
        }
        return rowSet;
    }

    private void addRow(final RowSet rowSet, final File file, final DateX dateX) {
        final String dateString = file.getName().replace(histogram.getName() + ".", "").replace(".xml", "");
        final Date date = DateX.fromFilenameMM(dateString);
        final String href = PathU.toDir(httpRequest.getBaseURI(), request.getContext(), Html.HTML, dateString);
        final TableViewLink link = new TableViewLink(UTF16.ICON_FILE, null, href);
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(link);
        insertRow.setNextColumn(dateX.toString(date));
        insertRow.setNextColumn(file.length());
        rowSet.add(insertRow.getRow());
    }
}
