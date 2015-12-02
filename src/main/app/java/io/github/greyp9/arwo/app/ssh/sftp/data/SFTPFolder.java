package io.github.greyp9.arwo.app.ssh.sftp.data;

import ch.ethz.ssh2.SFTPv3DirectoryEntry;
import ch.ethz.ssh2.SFTPv3FileAttributes;
import io.github.greyp9.arwo.core.date.DateU;
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

import java.sql.Types;
import java.util.Collection;

public class SFTPFolder {
    private final RowSet rowSet;

    public final RowSet getRowSet() {
        return rowSet;
    }

    /**
     * Store folder information, such that no additional connectivity to data endpoint is needed.
     */
    public SFTPFolder(final Collection<SFTPv3DirectoryEntry> directoryEntries, final RowSetMetaData metaData) {
        // "native" sort, in case none supplied by user
        final Sorts sorts = new Sorts(new Sort("type", true), new Sort("name", true));
        // load from source data
        this.rowSet = new RowSet(metaData, sorts, null);
        for (final SFTPv3DirectoryEntry directoryEntry : directoryEntries) {
            // skip spurious entries
            if (".".equals(directoryEntry.filename)) {
                directoryEntry.getClass();
            } else if ("..".equals(directoryEntry.filename)) {
                directoryEntry.getClass();
            } else {
                loadRow(rowSet, directoryEntry);
            }
        }
        rowSet.updateOrdinals();
    }

    public static RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData("type", Types.VARCHAR),
                new ColumnMetaData("name", Types.VARCHAR, true),
                new ColumnMetaData("mtime", Types.TIMESTAMP),
                new ColumnMetaData("ext", Types.VARCHAR),
                new ColumnMetaData("uid", Types.VARCHAR),
                new ColumnMetaData("gid", Types.VARCHAR),
                new ColumnMetaData("perms", Types.VARCHAR),
                new ColumnMetaData("size", Types.BIGINT),
        };
        return new RowSetMetaData("sftpFolderType", columns);
    }

    private static void loadRow(final RowSet rowSet, final SFTPv3DirectoryEntry directoryEntry) {
        // prep
        final SFTPv3FileAttributes attributes = directoryEntry.attributes;
        final Integer type = toType(attributes);
        final int typePrimitive = NumberU.toInt(type, 0);
        final boolean isDirectory = (SFTP.S_IFDIR == typePrimitive);
        final boolean isRegularFile = (SFTP.S_IFREG == typePrimitive);
        final boolean isSymlink = (SFTP.S_IFLNK == typePrimitive);
        final String extension = (isDirectory ? null : new FileX(directoryEntry.filename).getExtension());
        // populate columns
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(type);
        insertRow.setNextColumn(directoryEntry.filename);
        insertRow.setNextColumn(DateU.toDate(attributes.mtime));
        insertRow.setNextColumn(extension);
        insertRow.setNextColumn(attributes.uid);
        insertRow.setNextColumn(attributes.gid);
        insertRow.setNextColumn(attributes.getOctalPermissions());
        insertRow.setNextColumn(attributes.size);
        rowSet.add(insertRow.getRow());
        // folder properties
        final PropertiesX propertiesX = new PropertiesX(rowSet.getProperties());
        if (isDirectory) {
            propertiesX.addLong(Const.FOLDERS, 1L);
        } else if (isRegularFile) {
            propertiesX.addLong(Const.FILES, 1L);
            propertiesX.addLong(Const.BYTES, NumberU.toLong(attributes.size, 0L));
        } else if (isSymlink) {
            propertiesX.addLong(Const.SYMLINKS, 1L);
        }
    }

    public static int toType(final SFTPv3FileAttributes attributes) {
        final int permissions = getPermissions(attributes);
        //boolean isSymlink = attributes.isSymlink();  // bug in lib
        final boolean isSymlink = ((permissions & SFTP.S_IFLNK) == SFTP.S_IFLNK);
        int type = 0;
        if (attributes == null) {
            type = 0;
        } else if (attributes.isDirectory()) {
            type = SFTP.S_IFDIR;
        } else if (isSymlink) {
            type = SFTP.S_IFLNK;
        } else if (attributes.isRegularFile()) {
            type = SFTP.S_IFREG;
        }
        return type;
    }

    private static int getPermissions(final SFTPv3FileAttributes attributes) {
        int permissions;
        if (attributes == null) {
            permissions = 0;
        } else if (attributes.permissions == null) {
            permissions = 0;
        } else {
            permissions = attributes.permissions;
        }
        return permissions;
    }

    public static class Const {
        public static final String BYTES = "bytes";
        public static final String FILES = "files";
        public static final String FOLDERS = "folders";
        public static final String SYMLINKS = "symlinks";
    }
}
