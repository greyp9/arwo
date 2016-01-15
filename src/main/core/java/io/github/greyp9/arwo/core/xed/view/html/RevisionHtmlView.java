package io.github.greyp9.arwo.core.xed.view.html;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.file.meta.MetaFileFactory;
import io.github.greyp9.arwo.core.file.zip.ZipMetaData;
import io.github.greyp9.arwo.core.file.zip.ZipVolume;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.io.StreamU;
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
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.state.XedUserState;
import io.github.greyp9.arwo.core.xml.DocumentU;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.sql.Types;
import java.util.Collection;
import java.util.Date;

@SuppressWarnings("PMD.ExcessiveImports")
public class RevisionHtmlView extends HtmlView {
    private final XedUserState userState;
    private final Bundle bundle;

    public RevisionHtmlView(final XedRequest request) {
        super(request);
        this.userState = request.getState();
        this.bundle = request.getBundle();
    }

    @Override
    public final String addContentTo(final Element html) throws IOException {
        final RowSetMetaData metaData = createMetaData();
        final RowSet rowSet = createRowSet(metaData);
        final Locus locus = userState.getLocus();
        final String submitID = userState.getSubmitID();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final TableContext tableContext = new TableContext(viewState, submitID, Html.TABLE, bundle, locus);
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
        return bundle.getString("xed.revisionsType");
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[]{
                new ColumnMetaData("load", Types.VARCHAR),  // i18n
                new ColumnMetaData("date", Types.TIMESTAMP, true),  // i18n
                new ColumnMetaData("comment", Types.VARCHAR),  // i18n
                new ColumnMetaData("crc", Types.VARCHAR),  // i18n
                new ColumnMetaData("size", Types.BIGINT),  // i18n
        };
        return new RowSetMetaData("xed.revisionsType", columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData) throws IOException {
        final RowSet rowSet = new RowSet(metaData, null, null);
        final File file = getRequest().getSession().getFile();
        final File fileRevisions = new File(file.getParentFile(), file.getName() + ".zip");
        createRowsZip(rowSet, fileRevisions);
        createRowFile(rowSet, file);
        createRowSession(rowSet, getRequest().getSession());
        return rowSet;
    }

    private void createRowsZip(final RowSet rowSet, final File fileRevisions) throws IOException {
        if (fileRevisions.exists()) {
            final ZipVolume zipVolume = new ZipVolume(fileRevisions);
            final Collection<ZipMetaData> entries = zipVolume.getEntries();
            for (final ZipMetaData entry : entries) {
                createRow(rowSet, entry);
            }
        }
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

    private void createRowFile(final RowSet rowSet, final File file) throws IOException {
        final String labelOnDisk = bundle.getString("xed.revisionsType.diskRevision");
        final MetaFile metaFile = MetaFileFactory.create(file);
        if (metaFile != null) {
            final FileMetaData metaData = metaFile.getMetaData();
            final byte[] bytes = StreamU.read(metaFile.getBytes());
            final InsertRow insertRow = new InsertRow(rowSet);
            insertRow.setNextColumn(new TableViewButton(labelOnDisk, null, null));
            insertRow.setNextColumn(new Date(metaData.getLastModified()));
            insertRow.setNextColumn(null);
            insertRow.setNextColumn(Long.toHexString(CRCU.crc32(bytes)));
            insertRow.setNextColumn(bytes.length);
            rowSet.add(insertRow.getRow());
        }
    }

    private void createRowSession(final RowSet rowSet, final XedSession session) throws IOException {
        final String labelInMemory = bundle.getString("xed.revisionsType.memRevision");
        final byte[] bytes = DocumentU.toXmlPretty(session.getXed().getDocument());
        final Date date = Value.defaultOnNull(session.getDateModify(), session.getDateLoad());
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewButton(labelInMemory, null, null));
        insertRow.setNextColumn(date);
        insertRow.setNextColumn(null);
        insertRow.setNextColumn(Long.toHexString(CRCU.crc32(bytes)));
        insertRow.setNextColumn(bytes.length);
        rowSet.add(insertRow.getRow());
    }
}
