package io.github.greyp9.arwo.app.vis.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.vis.core.VisualizationRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogram;
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
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.util.Map;
import java.util.Properties;

public class VisualizationInventoryView extends VisualizationView {

    public VisualizationInventoryView(
            ServletHttpRequest httpRequest, VisualizationRequest request, AppUserState userState) {
        super(httpRequest, request, userState);
    }

    protected HttpResponse addContentTo(Element html) throws IOException {
        final RowSetMetaData metaData = createMetaData();
        final RowSet rowSet = createRowSet(metaData);
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
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
                new ColumnMetaData(App.Action.SELECT, Types.VARCHAR),
                new ColumnMetaData(App.Settings.NAME, Types.VARCHAR, true),
        };
        return new RowSetMetaData("vis.visualizationType", columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData) throws IOException {
        final RowSet rowSet = new RowSet(metaData, null, null);
        // available visualizations are declared in init-params of servlet
        final Properties initParams = httpRequest.getInitParams();
        for (final Map.Entry<Object, Object> entry : initParams.entrySet()) {
            final String key = (String) entry.getKey();
            final String value = (String) entry.getValue();
            if (TimeHistogram.class.getName().equals(value)) {
                createRow(rowSet, key);
            }
        }
        return rowSet;
    }

    private void createRow(final RowSet rowSet, final String key) {
        final String href = PathU.toDir(httpRequest.getBaseURI(), key);
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, null, href));
        insertRow.setNextColumn(key);
        rowSet.add(insertRow.getRow());
    }
}
