package io.github.greyp9.arwo.app.vis.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.vis.core.VisualizationRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.metric.histogram2.time.TimeHistogram;
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
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

public final class VisualizationInventoryView extends VisualizationView {

    public VisualizationInventoryView(
            final ServletHttpRequest httpRequest, final VisualizationRequest request, final AppUserState userState) {
        super(httpRequest, request, userState);
    }

    protected HttpResponse addContentTo(final Element html) throws IOException {
        final AppUserState userState = getUserState();
        final Bundle bundle = getBundle();
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
                new ColumnMetaData(App.Action.SELECT, Types.DATALINK),
                new ColumnMetaData(App.Settings.NAME, Types.VARCHAR, true),
        };
        return new RowSetMetaData("vis.visualizationType", columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData) {
        final ServletHttpRequest httpRequest = getHttpRequest();
        final RowSet rowSet = new RowSet(metaData, null, null);
        // available visualizations are declared in init-params of servlet
        final Properties initParams = httpRequest.getInitParams();
        final Set<Object> keys = new TreeSet<>(initParams.keySet());
        for (Object key : keys) {
            final String value = initParams.getProperty(key.toString());
            if (TimeHistogram.class.getName().equals(value)) {
                createRow(rowSet, key.toString());
            }
        }
        return rowSet;
    }

    private void createRow(final RowSet rowSet, final String key) {
        final ServletHttpRequest httpRequest = getHttpRequest();
        final String href = PathU.toDir(httpRequest.getBaseURI(), key);
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, null, href));
        insertRow.setNextColumn(key);
        rowSet.add(insertRow.getRow());
    }
}
