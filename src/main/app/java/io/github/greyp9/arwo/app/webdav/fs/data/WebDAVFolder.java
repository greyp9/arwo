package io.github.greyp9.arwo.app.webdav.fs.data;

import com.github.sardine.DavResource;
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

import java.sql.Types;
import java.util.Collection;

public class WebDAVFolder {
    private final RowSet rowSet;

    public final RowSet getRowSet() {
        return rowSet;
    }

    public WebDAVFolder(final String folder, final Collection<DavResource> directoryEntries,
                        final RowSetMetaData metaData, final boolean sort) {
        // "native" sort, in case none supplied by user
        final Sorts sorts = (sort ? new Sorts(new Sort("type", true), new Sort("name", true)) : null);
        // load from source data
        this.rowSet = new RowSet(metaData, sorts, null);
        for (final DavResource directoryEntry : directoryEntries) {
            // skip spurious entries
            if (directoryEntry.getPath().endsWith("/")) {  // identity entry
                directoryEntry.getClass();
            } else {
                loadRow(rowSet, folder, directoryEntry);
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
        return new RowSetMetaData("webdavFolderType", columns);
    }

    @SuppressWarnings("PMD.NPathComplexity")
    private static void loadRow(final RowSet rowSet, final String folder, final DavResource directoryEntry) {
        // prep
        final boolean isDirectory = directoryEntry.isDirectory();
        final Integer type = (isDirectory ? App.FS.S_IFDIR : App.FS.S_IFREG);
        final Long contentLengthRaw = directoryEntry.getContentLength();
        final Long contentLength = (contentLengthRaw == -1) ? null : contentLengthRaw;
        final String extension = (isDirectory ? null : new FileX(directoryEntry.getName()).getExtension());
        // populate columns
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(type);
        insertRow.setNextColumn(folder);
        insertRow.setNextColumn(directoryEntry.getName());
        insertRow.setNextColumn(directoryEntry.getModified());
        insertRow.setNextColumn(extension);
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
}
