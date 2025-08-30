package io.github.greyp9.arwo.app.cache.view;

import io.github.greyp9.arwo.app.cache.core.Cache;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.file.zip.ZipMetaData;
import io.github.greyp9.arwo.core.file.zip.ZipVolume;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;

import java.io.IOException;
import java.sql.Types;
import java.util.Date;

public final class MetaFileRowSetZip {
    private final String tableId;
    private final String baseURI;
    private final ZipVolume volume;

    public MetaFileRowSetZip(final String tableId, final String baseURI, final ZipVolume volume) {
        this.tableId = tableId;
        this.baseURI = PathU.toPath(baseURI, Cache.CONTEXT_METAFILES);
        this.volume = volume;
    }

    public RowSet getRowSet() throws IOException {
        final RowSet rowSet = new RowSet(createMetaData(), null, null);
        for (ZipMetaData metaData : volume.getEntries()) {
            addRow(rowSet, metaData);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[]{
                new ColumnMetaData(App.Attr.SELECT, Types.DATALINK),
                new ColumnMetaData(App.Attr.NAME, Types.VARCHAR, true),
                new ColumnMetaData(App.Attr.MTIME, Types.TIMESTAMP),
                new ColumnMetaData(App.Attr.EXTENSION, Types.VARCHAR),
                new ColumnMetaData(App.Attr.SIZE, Types.INTEGER),
        };
        return new RowSetMetaData(tableId, columns);
    }

    private void addRow(final RowSet rowSet, final ZipMetaData metaData) {
        final String id = rowSet.getID();
        final String path = baseURI + id + App.Token.BANG_SLASH + metaData.getPath();
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, App.Action.SELECT, path));
        insertRow.setNextColumn(metaData.getPath());
        insertRow.setNextColumn(new Date(metaData.getLastModified()));
        insertRow.setNextColumn(new FileX(metaData.getPath()).getExtension());
        insertRow.setNextColumn(metaData.getLength());
        rowSet.add(insertRow.getRow());
    }
}
