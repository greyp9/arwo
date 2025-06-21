package io.github.greyp9.arwo.app.local.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.CursorFolder;
import io.github.greyp9.arwo.core.config.CursorSetFolder;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
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
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.util.Collection;

public class LFSInventoryView extends LFSView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final String offsetURI;

    public LFSInventoryView(
            final LFSRequest request, final AppUserState userState, final String offsetURI) {
        super(request, userState, null, null);
        this.httpRequest = request.getHttpRequest();
        this.userState = userState;
        this.offsetURI = offsetURI;
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final Document document = html.getOwnerDocument();
        final Element header = new XPather(document, null).getElement(Html.XPath.HEADER);
        final Element content = new XPather(document, null).getElement(Html.XPath.CONTENT);

        super.addMenusLFS(header);

        final Xed xed = userState.getDocumentState().getSession(App.Servlet.SETTINGS).getXed();
        final Xed xedX = userState.getXedFactory().getXedUI(xed, userState.getLocale());
        final RowSetMetaData metaData = createMetaData();
        final RowSet rowSet = createRowSet(metaData, xedX);
        final Bundle bundle = xedX.getBundle();
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
        final TableContext tableContext = new TableContext(
                viewState, filter, userState.getSubmitID(), App.CSS.TABLE, bundle, locus);
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(content);
        return null;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(App.Action.SELECT, Types.DATALINK),
                new ColumnMetaData(App.Settings.NAME, Types.VARCHAR, true),
                new ColumnMetaData(App.Settings.COMMENT, Types.VARCHAR),
                new ColumnMetaData(App.Settings.FOLDER, Types.VARCHAR),
        };
        return new RowSetMetaData("folder.localFolderType", columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData, final Xed xed) throws IOException {
        final RowSet rowSet = new RowSet(metaData, null, null);
        final String baseURI = PathU.toPath(httpRequest.getBaseURI(), offsetURI);
        final Collection<XedCursor> cursors = new CursorSetFolder(xed).getCursors();
        for (final XedCursor cursor : cursors) {
            createRow(rowSet, baseURI, cursor);
        }
        return rowSet;
    }

    private void createRow(final RowSet rowSet, final String baseURI, final XedCursor cursor) {
        final CursorFolder cursorFolder = new CursorFolder(cursor);
        final boolean enabled = cursorFolder.isEnabled();
        if (enabled) {
            final String href = PathU.toDir(baseURI, cursorFolder.getName());
            final InsertRow insertRow = new InsertRow(rowSet);
            insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, null, href));
            insertRow.setNextColumn(cursorFolder.getName());
            insertRow.setNextColumn(cursorFolder.getComment());
            insertRow.setNextColumn(cursorFolder.getFolder());
            rowSet.add(insertRow.getRow());
        }
    }

}
