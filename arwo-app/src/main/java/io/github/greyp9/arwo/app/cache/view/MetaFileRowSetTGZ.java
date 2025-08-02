package io.github.greyp9.arwo.app.cache.view;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.file.tar.TarMetaData;
import io.github.greyp9.arwo.core.file.tar.TarVolume;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;

import java.io.IOException;
import java.sql.Types;
import java.util.Date;

public final class MetaFileRowSetTGZ {
    private final String tableId;
    private final String baseURI;
    private final TarVolume tarVolume;

    public MetaFileRowSetTGZ(final String tableId, final String baseURI, final TarVolume tarVolume) {
        this.tableId = tableId;
        this.baseURI = baseURI;
        this.tarVolume = tarVolume;
    }

    public RowSet getRowSet() throws IOException {
        final RowSet rowSet = new RowSet(createMetaData(), null, null);
        for (TarMetaData tarMetaData : tarVolume.getEntries()) {
            addRow(rowSet, tarMetaData);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[]{
                new ColumnMetaData("select", Types.DATALINK),
                new ColumnMetaData("name", Types.VARCHAR, true),
                new ColumnMetaData("mtime", Types.TIMESTAMP),
                new ColumnMetaData("extension", Types.VARCHAR),
                new ColumnMetaData("length", Types.INTEGER),
        };
        return new RowSetMetaData(tableId, columns);
    }

    private void addRow(final RowSet rowSet, final TarMetaData metaData) {
        final String id = rowSet.getID();
        final String path = baseURI + "/f" + id + "!/" + metaData.getPath();
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, App.Action.SELECT, path));
        insertRow.setNextColumn(metaData.getPath());
        insertRow.setNextColumn(new Date(metaData.getLastModified()));
        insertRow.setNextColumn(new FileX(metaData.getPath()).getExtension());
        insertRow.setNextColumn(metaData.getLength());
        rowSet.add(insertRow.getRow());
    }
}
