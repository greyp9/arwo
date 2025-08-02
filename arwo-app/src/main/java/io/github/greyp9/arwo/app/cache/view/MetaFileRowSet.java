package io.github.greyp9.arwo.app.cache.view;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.cell.TableViewButton;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;

import java.sql.Types;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;

public final class MetaFileRowSet {
    private final String tableId;
    private final String submitId;
    private final String baseURI;
    private final Iterator<Map.Entry<String, MetaFile>> iterator;

    public MetaFileRowSet(final String tableId,
                          final String submitId,
                          final String baseURI,
                          final Iterator<Map.Entry<String, MetaFile>> iterator) {
        this.tableId = tableId;
        this.submitId = submitId;
        this.baseURI = baseURI;
        this.iterator = iterator;
    }

    public RowSet getRowSet() {
        final RowSet rowSet = new RowSet(createMetaData(), null, null);
        while (iterator.hasNext()) {
            final Map.Entry<String, MetaFile> entry = iterator.next();
            addRow(rowSet, entry.getKey(), entry.getValue());
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[]{
                new ColumnMetaData("select", Types.DATALINK),
                new ColumnMetaData("refresh", Types.DATALINK),
                new ColumnMetaData("name", Types.VARCHAR, true),
                new ColumnMetaData("timestamp", Types.TIMESTAMP),
                new ColumnMetaData("extension", Types.VARCHAR),
                new ColumnMetaData("length", Types.INTEGER),
        };
        return new RowSetMetaData(tableId, columns);
    }

    private void addRow(final RowSet rowSet, final String key, final MetaFile metaFile) {
        final String hrefSelect = PathU.toPath(baseURI, key);
        final String hrefRefresh = metaFile.getMetaData().getProperties().getProperty("href");
        final SubmitToken tokenRefresh = new SubmitToken(App.Target.USER_STATE, App.Action.RELOAD, hrefRefresh);
        final FileMetaData metaData = metaFile.getMetaData();

        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, App.Action.SELECT, hrefSelect));
        insertRow.setNextColumn(new TableViewButton(UTF16.REFRESH, submitId, tokenRefresh.toString()));
        insertRow.setNextColumn(metaData.getPath());
        insertRow.setNextColumn(new Date(metaData.getLastModified()));
        insertRow.setNextColumn(new FileX(metaData.getPath()).getExtension());
        insertRow.setNextColumn(metaData.getLength());
        rowSet.add(insertRow.getRow());
    }
}
