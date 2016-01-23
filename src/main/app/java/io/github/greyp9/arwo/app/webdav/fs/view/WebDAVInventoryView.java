package io.github.greyp9.arwo.app.webdav.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.CursorSetWebDAV;
import io.github.greyp9.arwo.core.config.CursorWebDAV;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
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
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.util.Collection;

public class WebDAVInventoryView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final String offsetURI;

    public WebDAVInventoryView(
            final ServletHttpRequest httpRequest, final AppUserState userState, final String offsetURI) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.offsetURI = offsetURI;
    }

    public final void addContent(final Element html) throws IOException {
        final Xed xed = userState.getDocumentState().getSession(App.Servlet.SETTINGS).getXed();
        final RowSetMetaData metaData = createMetaData();
        final RowSet rowSet = createRowSet(metaData, xed);
        final Bundle bundle = xed.getBundle();
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final TableContext tableContext = new TableContext(
                viewState, userState.getSubmitID(), App.CSS.TABLE, bundle, locus);
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(App.Action.SELECT, Types.VARCHAR),
                new ColumnMetaData(App.Settings.NAME, Types.VARCHAR, true),
                new ColumnMetaData(App.Settings.COMMENT, Types.VARCHAR),
                new ColumnMetaData(App.Settings.USER, Types.VARCHAR),
                new ColumnMetaData(App.Settings.HOST, Types.VARCHAR),
                new ColumnMetaData(App.Settings.PORT, Types.VARCHAR),
        };
        return new RowSetMetaData("server.webdavServerType", columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData, final Xed xed) throws IOException {
        final RowSet rowSet = new RowSet(metaData, null, null);
        final String baseURI = PathU.toPath(httpRequest.getBaseURI(), offsetURI);
        final Collection<XedCursor> cursors = new CursorSetWebDAV(xed).getCursors();
        for (final XedCursor cursor : cursors) {
            createRow(rowSet, baseURI, cursor);
        }
        return rowSet;
    }

    private void createRow(final RowSet rowSet, final String baseURI, final XedCursor cursor) {
        final CursorWebDAV cursorIt = new CursorWebDAV(cursor);
        final boolean enabled = cursorIt.isEnabled();
        if (enabled) {
            final String href = PathU.toDir(baseURI, cursorIt.getName());
            final InsertRow insertRow = new InsertRow(rowSet);
            insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, null, href));
            insertRow.setNextColumn(cursorIt.getName());
            insertRow.setNextColumn(cursorIt.getComment());
            insertRow.setNextColumn(cursorIt.getUser());
            insertRow.setNextColumn(cursorIt.getHost());
            insertRow.setNextColumn(cursorIt.getPort());
            rowSet.add(insertRow.getRow());
        }
    }
}
