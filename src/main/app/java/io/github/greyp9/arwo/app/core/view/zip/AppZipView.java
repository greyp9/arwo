package io.github.greyp9.arwo.app.core.view.zip;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.file.zip.ZipMetaData;
import io.github.greyp9.arwo.core.file.zip.ZipVolume;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.locus.Locus;
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
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.sql.Types;
import java.util.Date;

@SuppressWarnings("PMD.ExcessiveImports")
public class AppZipView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final String zipEntry;


    public AppZipView(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        final NameTypeValues query = HttpArguments.toArguments(httpRequest.getHttpRequest().getQuery());
        this.zipEntry = query.getValue(Const.QUERY_ZIP_ENTRY);
    }

    public final HttpResponse addContentTo(
            final Element html, final MetaFile metaFile, final Bundle bundle) throws IOException {
        final RowSetMetaData metaData = createMetaData();
        final byte[] bytes = StreamU.read(metaFile.getBytes());
        final RowSet rowSet = createRowSet(metaData, bytes);
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final String title = httpRequest.getURI();
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), title, title);
        TableU.addFooterStandard(table, bundle);
        final TableContext tableContext = new TableContext(viewState, userState.getSubmitID(), "table", bundle, locus);
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
        return (HttpResponse) rowSet.getProperties().get(Const.QUERY_ZIP_ENTRY);
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData("type", Types.VARCHAR),
                new ColumnMetaData("name", Types.VARCHAR, true),
                new ColumnMetaData("mtime", Types.TIMESTAMP),
                new ColumnMetaData("ext", Types.VARCHAR),
                new ColumnMetaData("comment", Types.VARCHAR),
                new ColumnMetaData("crc", Types.VARCHAR),
                new ColumnMetaData("sizeCompress", Types.BIGINT),
                new ColumnMetaData("size", Types.BIGINT),
        };
        return new RowSetMetaData("zipFolderType", columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData, final byte[] bytes) throws IOException {
        final RowSet rowSet = new RowSet(metaData, null, null);
        final ZipVolume zipVolume = new ZipVolume(new ByteArrayInputStream(bytes));
        final MetaFile metaFile = zipVolume.getEntry(zipEntry);
        if (metaFile == null) {
            createRows(rowSet, zipVolume);
        } else {
            createResponse(rowSet, metaFile);
        }
        return rowSet;
    }

    private void createRows(final RowSet rowSet, final ZipVolume zipVolume) throws IOException {
        for (final ZipMetaData zipMetaData : zipVolume.getEntries()) {
            createRow(zipMetaData, rowSet);
        }
    }

    private void createRow(final ZipMetaData metaData, final RowSet rowSet) {
        final String href = String.format("?%s=%s", Const.QUERY_ZIP_ENTRY, metaData.getPath());
        final long crc = metaData.getCrc();
        final boolean isDataCRC = ((crc != -1) && (crc != 0));
        final String crcText = (isDataCRC ? NumberU.toHex((int) crc) : null);
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.ICON_FILE, null, href));
        insertRow.setNextColumn(metaData.getPath());
        insertRow.setNextColumn(new Date(metaData.getLastModified()));
        insertRow.setNextColumn(new FileX(metaData.getPath()).getExtension());
        insertRow.setNextColumn(metaData.getComment());
        insertRow.setNextColumn(crcText);
        insertRow.setNextColumn(metaData.getCompressedLength());
        insertRow.setNextColumn(metaData.getLength());
        rowSet.add(insertRow.getRow());
    }

    private void createResponse(final RowSet rowSet, final MetaFile metaFile) {
        final NameTypeValues headers = new NameTypeValues(
                new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8));
        final HttpResponse httpResponse = new HttpResponse(HttpURLConnection.HTTP_OK, headers, metaFile.getBytes());
        PropertiesU.setProperty(rowSet.getProperties(), Const.QUERY_ZIP_ENTRY, httpResponse);
    }

    public static class Const {
        public static final String QUERY_ZIP_ENTRY = "zipEntry";
    }
}