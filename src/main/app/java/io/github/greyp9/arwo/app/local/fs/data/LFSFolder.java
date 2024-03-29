package io.github.greyp9.arwo.app.local.fs.data;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.sort.Sort;
import io.github.greyp9.arwo.core.table.sort.Sorts;
import io.github.greyp9.arwo.core.util.PropertiesX;

import java.io.File;
import java.sql.Types;
import java.util.Date;
import java.util.logging.Logger;

public class LFSFolder {
    private final RowSet rowSet;

    public final RowSet getRowSet() {
        return rowSet;
    }

    public LFSFolder(final File folderBase, final String folderOffset, final File[] files,
                     final RowSetMetaData metaData, final boolean viewDot, final boolean sort) {
        // "native" sort, in case none supplied by user
        final Sorts sorts = (sort ? new Sorts(new Sort("type", true), new Sort("name", true)) : null);  // i18n metadata
        // load from source data
        this.rowSet = new RowSet(metaData, sorts, null);
        for (final File file : files) {
            final String fileName = file.getName();
            final boolean include = (viewDot || !fileName.startsWith("."));
            // skip spurious entries
            if (".".equals(fileName)) {
                file.getClass();
            } else if ("..".equals(fileName)) {
                file.getClass();
            } else if (include) {
                loadRow(rowSet, folderBase, folderOffset, file);
            }
        }
        rowSet.updateOrdinals(0);
    }

    public static RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData("type", Types.DATALINK),  // i18n metadata
                new ColumnMetaData("folder", Types.VARCHAR, true),  // i18n metadata
                new ColumnMetaData("name", Types.VARCHAR, true),  // i18n metadata
                new ColumnMetaData("mtime", Types.TIMESTAMP),  // i18n metadata
                new ColumnMetaData("ext", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("size", Types.BIGINT),  // i18n metadata
        };
        return new RowSetMetaData("lfsFolderType", columns);  // i18n metadata
    }

    private static void loadRow(
            final RowSet rowSet, final File folderBase, final String folderOffset, final File file) {
        Logger.getLogger(LFSFolder.class.getName()).finest(folderOffset);
        final String pathFileIt = file.getAbsolutePath();
        final String pathOffset = pathFileIt.substring(folderBase.getAbsolutePath().length());
        final String pathFolder = pathOffset.substring(0, pathOffset.length() - file.getName().length());
        // prep
        final boolean isLink = FileU.isLink(file);
        final boolean isDirectory = file.isDirectory();
        final Integer type = (isLink ? App.FS.S_IFLNK : (isDirectory ? App.FS.S_IFDIR : App.FS.S_IFREG));
        final String extension = (isDirectory ? null : new FileX(file.getName()).getExtension());
        // populate columns
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(type);
        insertRow.setNextColumn(pathFolder);
        insertRow.setNextColumn(file.getName());  // (isLink ? file.getAbsolutePath() : file.getName());
        insertRow.setNextColumn(new Date(file.lastModified()));
        insertRow.setNextColumn(extension);
        insertRow.setNextColumn(file.length());
        rowSet.add(insertRow.getRow());
        // folder properties
        final PropertiesX propertiesX = new PropertiesX(rowSet.getProperties());
        if (isDirectory) {
            propertiesX.addLong(App.FS.FOLDERS, 1L);
        } else {
            propertiesX.addLong(App.FS.FILES, 1L);
            propertiesX.addLong(App.FS.BYTES, NumberU.toLong(file.length(), 0L));
        }
    }
}
