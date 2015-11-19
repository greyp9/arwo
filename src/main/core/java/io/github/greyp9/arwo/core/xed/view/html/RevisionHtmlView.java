package io.github.greyp9.arwo.core.xed.view.html;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.zip.ZipMetaData;
import io.github.greyp9.arwo.core.file.zip.ZipVolume;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.cell.TableViewButton;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.state.XedUserState;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.sql.Types;
import java.util.Collection;
import java.util.Date;

public class RevisionHtmlView extends HtmlView {
    private final XedUserState userState;

    public RevisionHtmlView(final XedRequest request) {
        super(request);
        this.userState = request.getState();
    }

    @Override
    public final void addContentTo(final Element html) throws IOException {
        final RowSetMetaData metaData = createMetaData();
        final RowSet rowSet = createRowSet(metaData);
        final Locus locus = userState.getLocus();
        final String submitID = userState.getSubmitID();
        final Bundle bundle = getRequest().getBundle();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final TableContext tableContext = new TableContext(viewState, submitID, "table", bundle, locus);
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[]{
                new ColumnMetaData("load", Types.VARCHAR),
                new ColumnMetaData("date", Types.TIMESTAMP, true),
                new ColumnMetaData("comment", Types.VARCHAR),
                new ColumnMetaData("crc", Types.VARCHAR),
                new ColumnMetaData("size", Types.BIGINT),
        };
        return new RowSetMetaData("xed.revisionsType", columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData) throws IOException {
        final RowSet rowSet = new RowSet(metaData, null, null);
        final File file = getRequest().getSession().getFile();
        final File fileRevisions = new File(file.getParentFile(), file.getName() + ".zip");
        final ZipVolume zipVolume = new ZipVolume(fileRevisions);
        final Collection<ZipMetaData> entries = zipVolume.getEntries();
        for (final ZipMetaData entry : entries) {
            createRow(rowSet, entry);
        }
        return rowSet;
    }

    private void createRow(final RowSet rowSet, final ZipMetaData entry) throws IOException {
        final SubmitToken tokenLoad = new SubmitToken(App.Target.SESSION, App.Action.LOAD_REVISION, entry.getPath());
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewButton(UTF16.SELECT, userState.getSubmitID(), tokenLoad.toString()));
        insertRow.setNextColumn(new Date(entry.getLastModified()));
        insertRow.setNextColumn(entry.getComment());
        insertRow.setNextColumn(Long.toHexString(entry.getCrc()));
        insertRow.setNextColumn(entry.getLength());
        rowSet.add(insertRow.getRow());
    }
}
