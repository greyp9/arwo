package io.github.greyp9.arwo.app.webdav.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.webdav.connection.WebDAVConnectionResource;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
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
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.lib.sardine.webdav.connection.WebDAVConnection;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.util.Collection;

public class WebDAVConnectionsView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final String offsetURI;
    private final boolean select;

    public WebDAVConnectionsView(final ServletHttpRequest httpRequest, final AppUserState userState,
                                 final String offsetURI, final boolean select) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.offsetURI = offsetURI;
        this.select = select;
    }

    public final void addContent(final Element html) throws IOException {
        final Xed xed = userState.getDocumentState().getSession("/app").getXed();
        final RowSetMetaData metaData = createMetaData();
        final RowSet rowSet = createRowSet(metaData);
        final Bundle bundle = xed.getBundle();
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final TableContext tableContext = new TableContext(viewState, userState.getSubmitID(), "table", bundle, locus);
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(App.Action.SELECT, Types.VARCHAR),
                new ColumnMetaData("name", Types.VARCHAR, true),
                new ColumnMetaData("hashCode", Types.VARCHAR),
                new ColumnMetaData("opened", Types.TIMESTAMP),
                new ColumnMetaData("last", Types.TIMESTAMP),
                new ColumnMetaData("count", Types.INTEGER),
                new ColumnMetaData("millis", Types.INTEGER),
                new ColumnMetaData("close", Types.VARCHAR),
        };
        return new RowSetMetaData("webdavConnectionType", columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData) throws IOException {
        final RowSet rowSet = new RowSet(metaData, null, null);
        final String baseURI = PathU.toPath(httpRequest.getBaseURI(), offsetURI);
        final Collection<ConnectionResource> resources = userState.getWebDAV().getCache().getResources();
        for (final ConnectionResource resource : resources) {
            createRow(rowSet, baseURI, resource);
        }
        return rowSet;
    }

    private void createRow(final RowSet rowSet, final String baseURI, final ConnectionResource resource) {
        final WebDAVConnection connection = ((WebDAVConnectionResource) resource).getConnection();
        final String href = PathU.toDir(baseURI, resource.getName());
        final String hashCode = Integer.toHexString(connection.hashCode());
        final SubmitToken tokenClose = new SubmitToken(
                App.Target.USER_STATE, App.Action.CLOSE, "webdav", resource.getName());
        final InsertRow insertRow = new InsertRow(rowSet);
        final Object cellSelect = (select ? new TableViewLink(UTF16.SELECT, null, href) : null);
        insertRow.setNextColumn(cellSelect);
        insertRow.setNextColumn(resource.getName());
        insertRow.setNextColumn(hashCode);
        insertRow.setNextColumn(connection.getDateOpen());
        insertRow.setNextColumn(connection.getDateLast());
        insertRow.setNextColumn(connection.getCount());
        insertRow.setNextColumn(new Duration(connection.getMillis()));
        insertRow.setNextColumn(new TableViewButton(UTF16.CLOSE, userState.getSubmitID(), tokenClose.toString()));
        rowSet.add(insertRow.getRow());
    }
}
