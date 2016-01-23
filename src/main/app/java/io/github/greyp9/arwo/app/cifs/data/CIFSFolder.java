package io.github.greyp9.arwo.app.cifs.data;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.sort.Sort;
import io.github.greyp9.arwo.core.table.sort.Sorts;
import io.github.greyp9.arwo.core.util.PropertiesX;
import jcifs.smb.SmbException;
import jcifs.smb.SmbFile;

import java.sql.Types;
import java.util.Collection;
import java.util.Date;

public class CIFSFolder {
    private final RowSet rowSet;

    public final RowSet getRowSet() {
        return rowSet;
    }

    public CIFSFolder(final String folder, final Collection<SmbFile> directoryEntries,
                      final RowSetMetaData metaData, final boolean sort) throws SmbException {
        // "native" sort, in case none supplied by user
        final Sorts sorts = (sort ? new Sorts(new Sort("type", true), new Sort("name", true)) : null);  // i18n metadata
        // load from source data
        this.rowSet = new RowSet(metaData, sorts, null);
        for (final SmbFile directoryEntry : directoryEntries) {
            loadRow(rowSet, folder, directoryEntry);





        }
        rowSet.updateOrdinals(0);
    }

    public static RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData("type", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("folder", Types.VARCHAR, true),  // i18n metadata
                new ColumnMetaData("name", Types.VARCHAR, true),  // i18n metadata
                new ColumnMetaData("mtime", Types.TIMESTAMP),  // i18n metadata
                new ColumnMetaData("ext", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("uid", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("gid", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("perms", Types.VARCHAR),  // i18n metadata
                new ColumnMetaData("size", Types.BIGINT),  // i18n metadata
        };
        return new RowSetMetaData("cifsFolderType", columns);  // i18n metadata
    }

    @SuppressWarnings("PMD.NPathComplexity")
    private static void loadRow(
            final RowSet rowSet, final String folder, final SmbFile directoryEntry) throws SmbException {
        // prep
        final boolean isDirectory = directoryEntry.isDirectory();
        final Integer type = (isDirectory ? App.FS.S_IFDIR : App.FS.S_IFREG);
        final int contentLengthRaw = directoryEntry.getContentLength();
        final Long contentLength = (contentLengthRaw == -1) ? null : (long) contentLengthRaw;
        final String extension = (isDirectory ? null : new FileX(directoryEntry.getName()).getExtension());
        // populate columns
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(type);
        insertRow.setNextColumn(folder);
        insertRow.setNextColumn(directoryEntry.getName());
        insertRow.setNextColumn(new Date(directoryEntry.getLastModified()));
        insertRow.setNextColumn(extension);
        insertRow.setNextColumn(null);
        insertRow.setNextColumn(null);
        insertRow.setNextColumn(toPermissionsDisplay(directoryEntry));
        insertRow.setNextColumn(contentLength);
        rowSet.add(insertRow.getRow());
        // folder properties
        final PropertiesX propertiesX = new PropertiesX(rowSet.getProperties());
        if (isDirectory) {
            propertiesX.addLong(App.FS.FOLDERS, 1L);
        } else {
            propertiesX.addLong(App.FS.FILES, 1L);
            propertiesX.addLong(App.FS.BYTES, NumberU.toLong(contentLength, 0L));
        }
    }

    private static String toPermissionsDisplay(final SmbFile directoryEntry) throws SmbException {
        final boolean canRead = directoryEntry.canRead();
        final boolean canWrite = directoryEntry.canWrite();
        return ((canRead ? "R" : "") + (canWrite ? "W" : ""));
    }
}
