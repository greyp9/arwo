package io.github.greyp9.arwo.core.file.meta;

import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;
import java.sql.Types;

public final class MetaFolderRowSetSource implements RowSetSource {

    private final String rowSetId;
    private final MetaFolder metaFolder;

    public MetaFolderRowSetSource(final String rowSetId, final MetaFolder metaFolder) {
        this.rowSetId = rowSetId;
        this.metaFolder = metaFolder;
    }

    @Override
    public String getRowSetId() {
        return rowSetId;
    }

    @Override
    public RowSet getRowSet() throws IOException {
        final RowSet rowSet = new RowSet(createMetaData(), null, null);
        for (FileMetaData file : metaFolder.getFiles()) {
            loadRow(rowSet, file);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData("select", Types.VARCHAR),
                new ColumnMetaData("name", Types.VARCHAR, true),
                new ColumnMetaData("perms", Types.VARCHAR),
                new ColumnMetaData("owner", Types.VARCHAR),
                new ColumnMetaData("group", Types.VARCHAR),
                new ColumnMetaData("size", Types.INTEGER),
        };
        return new RowSetMetaData(rowSetId, columns);
    }

    private void loadRow(final RowSet rowSet, final FileMetaData file) {
        final FileMetaData.Type fsEntryType = file.getType();
        final String typeText = FileMetaData.toTypeText(fsEntryType);
        final String href;
        if (FileMetaData.Type.DIRECTORY.equals(fsEntryType)) {
            href = PathU.toDir(metaFolder.getLocation(), file.getPath());
        } else if (FileMetaData.Type.LINK.equals(fsEntryType)) {
            final String link = Value.defaultOnNull(file.getProperty("link"), file.getPath());
            href = PathU.toDir(metaFolder.getLocation(), link);
        } else {
            href = PathU.toPath(metaFolder.getLocation(), file.getPath());
        }

        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(typeText, fsEntryType.name(), href));
        insertRow.setNextColumn(file.getPath());
        insertRow.setNextColumn(file.getProperty(FileMetaData.PERMS));
        insertRow.setNextColumn(file.getProperty(FileMetaData.OWNER));
        insertRow.setNextColumn(file.getProperty(FileMetaData.GROUP));
        insertRow.setNextColumn(file.getLength());
        rowSet.add(insertRow.getRow());
    }
}
