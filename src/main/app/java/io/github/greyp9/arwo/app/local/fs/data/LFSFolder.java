package io.github.greyp9.arwo.app.local.fs.data;

import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.sort.Sort;
import io.github.greyp9.arwo.core.table.sort.Sorts;
import io.github.greyp9.arwo.core.util.PropertiesX;
import io.github.greyp9.arwo.lib.ganymed.ssh.core.SFTP;

import java.io.File;
import java.sql.Types;
import java.util.Date;

public class LFSFolder {
    private final RowSet rowSet;

    public final RowSet getRowSet() {
        return rowSet;
    }

    public LFSFolder(final String folder, final File[] files,
                     final RowSetMetaData metaData, final boolean sort) {
        // "native" sort, in case none supplied by user
        final Sorts sorts = (sort ? new Sorts(new Sort("type", true), new Sort("name", true)) : null);
        // load from source data
        this.rowSet = new RowSet(metaData, sorts, null);
        for (final File file : files) {
            // skip spurious entries
            if (".".equals(file.getName())) {
                file.getClass();
            } else if ("..".equals(file.getName())) {
                file.getClass();
            } else {
                loadRow(rowSet, folder, file);
            }
        }
        rowSet.updateOrdinals(0);
    }

    public static RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData("type", Types.VARCHAR),
                new ColumnMetaData("folder", Types.VARCHAR, true),
                new ColumnMetaData("name", Types.VARCHAR, true),
                new ColumnMetaData("mtime", Types.TIMESTAMP),
                new ColumnMetaData("ext", Types.VARCHAR),
                new ColumnMetaData("size", Types.BIGINT),
        };
        return new RowSetMetaData("lfsFolderType", columns);
    }

    public static void loadRow(final RowSet rowSet, final String folder, final File file) {
        // prep
        final boolean isDirectory = file.isDirectory();
        final Integer type = (isDirectory ? SFTP.S_IFDIR : SFTP.S_IFREG);
        final String extension = (isDirectory ? null : new FileX(file.getName()).getExtension());
        // populate columns
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(type);
        insertRow.setNextColumn(folder);
        insertRow.setNextColumn(file.getName());
        insertRow.setNextColumn(new Date(file.lastModified()));
        insertRow.setNextColumn(extension);
        insertRow.setNextColumn(file.length());
        rowSet.add(insertRow.getRow());
        // folder properties
        final PropertiesX propertiesX = new PropertiesX(rowSet.getProperties());
        if (isDirectory) {
            propertiesX.addLong(Const.FOLDERS, 1L);
        } else {
            propertiesX.addLong(Const.FILES, 1L);
            propertiesX.addLong(Const.BYTES, NumberU.toLong(file.length(), 0L));
        }
    }

    public static class Const {
        public static final String BYTES = "bytes";
        public static final String FILES = "files";
        public static final String FOLDERS = "folders";
    }
}
