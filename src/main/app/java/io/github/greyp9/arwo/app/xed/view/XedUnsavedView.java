package io.github.greyp9.arwo.app.xed.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppText;
import io.github.greyp9.arwo.core.bundle.Bundle;
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
import io.github.greyp9.arwo.core.xed.session.XedEntry;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.state.XedUserState;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.util.Collection;
import java.util.Locale;

public class XedUnsavedView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public XedUnsavedView(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final void addContent(final Element html) throws IOException {
        final Locale locale = userState.getLocus().getLocale();
        final Bundle bundle = new Bundle(new AppText(locale).getBundleCore());
        final RowSetMetaData metaData = createMetaData();
        final RowSet rowSet = createRowSet(metaData);
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
                new ColumnMetaData("url", Types.VARCHAR, true),
                new ColumnMetaData("title", Types.VARCHAR, true),
                new ColumnMetaData("opened", Types.TIMESTAMP),
                new ColumnMetaData("modified", Types.TIMESTAMP),
        };
        return new RowSetMetaData("xed.unsavedType", columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData) throws IOException {
        final RowSet rowSet = new RowSet(metaData, null, null);
        final XedUserState documentState = userState.getDocumentState();
        final Collection<XedSession> sessions = documentState.getSessions().getSessions();
        for (final XedSession session : sessions) {
            createRow(rowSet, session);
        }
        return rowSet;
    }

    private void createRow(final RowSet rowSet, final XedSession session) {
        if (session.getDateModify() != null) {
            final XedEntry entry = session.getEntry();
            final InsertRow insertRow = new InsertRow(rowSet);
            final String href = PathU.toDir(httpRequest.getContextPath() + entry.getContextPath());
            insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, null, href));
            insertRow.setNextColumn(entry.getContextPath());
            insertRow.setNextColumn(entry.getTitle());
            insertRow.setNextColumn(session.getDateLoad());
            insertRow.setNextColumn(session.getDateModify());
            rowSet.add(insertRow.getRow());
        }
    }
}
