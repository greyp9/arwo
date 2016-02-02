package io.github.greyp9.arwo.app.core.view.connect;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.connect.ConnectionResource;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.cell.Duration;
import io.github.greyp9.arwo.core.table.cell.TableViewButton;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaDataU;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;

public class AppConnectionView {
    private final AppRequest request;
    private final AppUserState userState;
    private final ConnectionCache cache;
    private final String baseURI;

    public AppConnectionView(final ServletHttpRequest httpRequest, final AppUserState userState,
                             final ConnectionCache cache) {
        this(httpRequest, userState, cache, null);
    }

    public AppConnectionView(final ServletHttpRequest httpRequest, final AppUserState userState,
                             final ConnectionCache cache, final String baseURI) {
        this.request = userState.getAppRequest(httpRequest);
        this.userState = userState;
        this.cache = cache;
        this.baseURI = baseURI;
    }

    public final void addContentTo(final Element html, final boolean displayOnEmpty) throws IOException {
        final RowSetMetaData metaData = createMetaData();
        final RowSet rowSet = createRowSet(metaData);
        if ((displayOnEmpty) || (rowSet.getRows() > 0)) {
            final Bundle bundle = request.getBundle();
            final Locus locus = request.getLocus();
            final String submitID = request.getSubmitID();
            final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
            final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
            TableU.addFooterStandard(table, bundle);
            final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
            final TableContext tableContext = new TableContext(
                    viewState, filter, submitID, App.CSS.TABLE, bundle, locus);
            final TableView tableView = new TableView(table, tableContext);
            tableView.addContentTo(html);
        }
    }

    private RowSetMetaData createMetaData() {
        final String id = cache.getName() + "ConnectionType";  // i18n metadata
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData("name", Types.VARCHAR, true),  // i18n metadata
                new ColumnMetaData("hashCode", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("opened", Types.TIMESTAMP),  // i18n metadata
                new ColumnMetaData("last", Types.TIMESTAMP),  // i18n metadata
                new ColumnMetaData("count", Types.INTEGER),  // i18n metadata
                new ColumnMetaData("millis", Types.INTEGER),  // i18n metadata
                new ColumnMetaData("close", Types.VARCHAR),  // i18n metadata
        };
        RowSetMetaData metaData = new RowSetMetaData(id, columns);
        if (baseURI != null) {
            metaData = RowSetMetaDataU.addLeft(metaData, new ColumnMetaData(App.Action.SELECT, Types.VARCHAR));
        }
        return metaData;
    }

    private RowSet createRowSet(final RowSetMetaData metaData) throws IOException {
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final ConnectionResource resource : cache.getResources()) {
            createRow(rowSet, resource);
        }
        return rowSet;
    }

    private void createRow(final RowSet rowSet, final ConnectionResource resource) {
        final SubmitToken tokenClose = new SubmitToken(
                App.Target.USER_STATE, App.Action.CLOSE, cache.getName(), resource.getName());
        final InsertRow insertRow = new InsertRow(rowSet);
        if (baseURI != null) {
            final String href = PathU.toDir(baseURI, resource.getName());
            insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, null, href));
        }
        insertRow.setNextColumn(resource.getName());
        insertRow.setNextColumn(resource.getID());
        insertRow.setNextColumn(resource.getDateOpen());
        insertRow.setNextColumn(resource.getDateLast());
        insertRow.setNextColumn(resource.getCount());
        insertRow.setNextColumn(new Duration(resource.getMillis()));
        insertRow.setNextColumn(new TableViewButton(UTF16.CLOSE, request.getSubmitID(), tokenClose.toString()));
        rowSet.add(insertRow.getRow());
    }
}
