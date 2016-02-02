package io.github.greyp9.arwo.core.xed.view.html;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.table.XedMetaDataFactory;
import io.github.greyp9.arwo.core.xed.table.XedRowSetFactory;
import io.github.greyp9.arwo.core.xed.view.XedTableView;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;

public class TableHtmlView {
    private final XedTableView view;
    private final XedRequest request;

    public TableHtmlView(final XedTableView view, final XedRequest request) {
        this.view = view;
        this.request = request;
    }

    public final void addContentTo(final Element html) throws IOException {
        final String baseURI = view.getBaseURI();
        final String submitID = request.getState().getSubmitID();
        final Locus locus = request.getState().getLocus();
        final XedCursor cursorSelected = view.getCursor();
        final boolean isTableType = (cursorSelected.getElement() == null);
        final XedCursor cursorTableType = (isTableType ? cursorSelected : cursorSelected.getParent());
        final TypeInstance typeInstance = cursorSelected.getTypeInstance();
        // enumerate raw table content
        final RowSetMetaData metaData = new XedMetaDataFactory().create(typeInstance, true);
        final XedRowSetFactory rowSetFactory = new XedRowSetFactory(
                baseURI, submitID, typeInstance, metaData, null, null);
        final XedCursor cursorConcrete = cursorSelected.getParentConcrete();
        final Collection<Element> children = cursorConcrete.getChildren(typeInstance);
        final RowSet rowSet = rowSetFactory.create(cursorConcrete, children, cursorSelected.getElement());
        // prep for render
        final Bundle bundle = cursorSelected.getXed().getBundle();
        final ViewStates viewStates = request.getState().getViewStates();
        final ViewState viewState = viewStates.getViewState(rowSet.getMetaData(), bundle, locus);
        viewState.setPage(Page.Factory.fixPage(viewState.getPage(), rowSet.getRows()));
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        addFooter(table, cursorTableType, bundle);
        final XedActionFilter filter = new XedActionFilter(request.getFactory(), request.getLocale());
        final TableContext context = new TableContext(viewState, filter, submitID, App.CSS.TABLE, bundle, locus);
        // render
        final TableView tableView = new TableView(table, context);
        tableView.addContentTo(html);
    }

    private void addFooter(final Table table, final XedCursor cursorTableType, final Bundle bundle) {
        // insert row link
        final String resource = view.getBaseURI() + cursorTableType.getURI();
        table.getProperties().setProperty(Table.Const.FOOTER_HREF_L, resource);
        table.getProperties().setProperty(Table.Const.FOOTER_L, UTF16.CREATE);
        // row count text
        TableU.addFooterStandard(table, bundle);
    }
}
