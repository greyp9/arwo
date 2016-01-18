package io.github.greyp9.arwo.app.core.view.favorite;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.cell.TableViewButton;
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
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.table.XedMetaDataFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.util.Collection;
import java.util.Iterator;

@SuppressWarnings("PMD.ExcessiveImports")
public class AppFavoriteView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final XedCursor cursorType;
    private final Bundle bundle;
    private final String type;

    public AppFavoriteView(final ServletHttpRequest httpRequest, final AppUserState userState,
                           final XedCursor cursorType, final String type) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.cursorType = cursorType;
        this.bundle = ((cursorType == null) ? null : cursorType.getXed().getBundle());
        this.type = type;
    }

    public final HttpResponse addContentTo(final Element html) throws IOException {
        final String menuKey = Value.join(Http.Token.SLASH, httpRequest.getServletPath(), type, "favorites");
        if ((cursorType != null) && (userState.getMenuSystem().isOpen(menuKey))) {
            addContentInner(html);
        }
        return null;
    }

    private void addContentInner(final Element html) throws IOException {
        final RowSetMetaData metaData = createMetaData();
        final RowSet rowSet = createRowSet(metaData);
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final TableContext tableContext = new TableContext(
                viewState, userState.getSubmitID(), App.CSS.TABLE, bundle, userState.getLocus());
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
    }

    private RowSetMetaData createMetaData() {
        final RowSetMetaData metaData = new XedMetaDataFactory().create(cursorType.getTypeInstance(), false);
        final ColumnMetaData columnAction = new ColumnMetaData(App.Action.SELECT, Types.VARCHAR, false);
        return RowSetMetaDataU.addLeft(metaData, columnAction);
    }

    private RowSet createRowSet(final RowSetMetaData metaData) throws IOException {
        final RowSet rowSet = new RowSet(metaData, null, null);
        final TypeInstance typeInstance = cursorType.getTypeInstance();
        final XedCursor parentConcrete = cursorType.getParentConcrete();
        final XedNav nav = new XedNav(cursorType.getXed());
        final Collection<Element> children = parentConcrete.getChildren(typeInstance);
        for (final Element child : children) {
            final XedCursor childCursor = nav.find(child, parentConcrete);
            createRow(rowSet, metaData, childCursor);
        }
        return rowSet;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private void createRow(final RowSet rowSet, final RowSetMetaData metaData, final XedCursor item) {
        final InsertRow insertRow = new InsertRow(rowSet);
        for (final Iterator<ColumnMetaData> it = metaData.iterator(); it.hasNext(); it.getClass()) {
            final ColumnMetaData columnMetaData = it.next();
            if (columnMetaData.getName().equals(App.Action.SELECT)) {
                final SubmitToken token = new SubmitToken(App.Target.SESSION, App.Action.SELECT_FAV, item.getURI());
                insertRow.setNextColumn(new TableViewButton(UTF16.SELECT, userState.getSubmitID(), token.toString()));
            } else {
                final TypeInstance typeInstance = item.getTypeInstance().getInstance(columnMetaData.getName());
                insertRow.setNextColumn(item.getValue(typeInstance));
            }
        }
        rowSet.add(insertRow.getRow());
    }
}
