package io.github.greyp9.arwo.app.ssh.sftp.data;

import ch.ethz.ssh2.SFTPv3DirectoryEntry;
import ch.ethz.ssh2.SFTPv3FileAttributes;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.date.DateConvertU;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.sort.Sort;
import io.github.greyp9.arwo.core.table.sort.Sorts;
import io.github.greyp9.arwo.core.util.PropertiesX;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnectionX;

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
    public SFTPFolder(final String folder, final Collection<SFTPv3DirectoryEntry> directoryEntries,
                      final RowSetMetaData metaData, final boolean sort, final SSHConnectionX sshConnectionX) {
        // "native" sort, in case none supplied by user
        final Sorts sorts = (sort ? new Sorts(new Sort("type", true), new Sort("name", true)) : null);  // i18n metadata
        // load from source data
        this.rowSet = new RowSet(metaData, sorts, null);
        for (final SFTPv3DirectoryEntry directoryEntry : directoryEntries) {
            // skip spurious entries
            if (".".equals(directoryEntry.filename)) {
                directoryEntry.getClass();
            } else if ("..".equals(directoryEntry.filename)) {
                directoryEntry.getClass();
            } else {
                loadRow(rowSet, folder, directoryEntry, sshConnectionX);
            }
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
        return new RowSetMetaData("sftpFolderType", columns);  // i18n metadata
    }

    private static void loadRow(final RowSet rowSet, final String folder,
                                final SFTPv3DirectoryEntry directoryEntry, final SSHConnectionX sshConnectionX) {
        // prep
        final SFTPv3FileAttributes attributes = directoryEntry.attributes;
        final Integer type = toType(attributes);
        final int typePrimitive = NumberU.toInt(type, 0);
        final boolean isDirectory = (App.FS.S_IFDIR == typePrimitive);
        final boolean isRegularFile = (App.FS.S_IFREG == typePrimitive);
        final boolean isSymlink = (App.FS.S_IFLNK == typePrimitive);
        final String extension = (isDirectory ? null : new FileX(directoryEntry.filename).getExtension());
        // populate columns
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(type);
        insertRow.setNextColumn(folder);
        insertRow.setNextColumn(directoryEntry.filename);
        insertRow.setNextColumn(DateConvertU.fromSeconds(attributes.mtime));
        insertRow.setNextColumn(extension);
        insertRow.setNextColumn(sshConnectionX.toNameUID(attributes.uid));
        insertRow.setNextColumn(sshConnectionX.toNameGID(attributes.gid));
        insertRow.setNextColumn(attributes.getOctalPermissions());
        insertRow.setNextColumn(attributes.size);
        rowSet.add(insertRow.getRow());
        // folder properties
        final PropertiesX propertiesX = new PropertiesX(rowSet.getProperties());
        if (isDirectory) {
            propertiesX.addLong(App.FS.FOLDERS, 1L);
        } else if (isRegularFile) {
            propertiesX.addLong(App.FS.FILES, 1L);
            propertiesX.addLong(App.FS.BYTES, NumberU.toLong(attributes.size, 0L));
        } else if (isSymlink) {
            propertiesX.addLong(App.FS.SYMLINKS, 1L);
        }
    }

    public static int toType(final SFTPv3FileAttributes attributes) {
        final int permissions = getPermissions(attributes);
        //boolean isSymlink = attributes.isSymlink();  // bug in lib
        final boolean isSymlink = ((permissions & App.FS.S_IFLNK) == App.FS.S_IFLNK);
        int type = 0;
        if (attributes == null) {
            type = 0;
        } else if (attributes.isDirectory()) {
            type = App.FS.S_IFDIR;
        } else if (isSymlink) {
            type = App.FS.S_IFLNK;
        } else if (attributes.isRegularFile()) {
            type = App.FS.S_IFREG;
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
}
