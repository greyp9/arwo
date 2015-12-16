package io.github.greyp9.arwo.app.ssh.sftp.data;

import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.util.PropertiesX;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.lib.ganymed.ssh.core.SFTP;

import java.io.UnsupportedEncodingException;
import java.util.Iterator;

public class SFTPFolderStyled {
    private final SFTPRequest request;
    private final RowSet rowSet;

    public final RowSet getRowSet() {
        return rowSet;
    }

    public SFTPFolderStyled(final SFTPRequest request, final RowSet rowSetRaw) throws UnsupportedEncodingException {
        this.request = request;
        this.rowSet = new RowSet(rowSetRaw.getMetaData(), null, null);
        final Iterator<Row> iterator = rowSetRaw.iterator();
        while (iterator.hasNext()) {
            loadRow(this.rowSet, iterator.next());
        }
        this.rowSet.getProperties().putAll(rowSetRaw.getProperties());
        addFooter(this.rowSet, request.getBundle());
    }

    private void loadRow(final RowSet rowSetStyled, final Row rowRaw) throws UnsupportedEncodingException {
        // input
        final RowSetMetaData metaData = rowSetStyled.getMetaData();
        final Integer type = rowRaw.getInteger(metaData.getIndex("type"));
        final String folder = rowRaw.getString(metaData.getIndex("folder"));
        final String name = rowRaw.getString(metaData.getIndex("name"));
        // processing
        final boolean isDirectory = (SFTP.S_IFDIR == NumberU.toInt(type, 0));
        // output
        final InsertRow insertRow = new InsertRow(rowSetStyled);
        insertRow.setNextColumn(getTypeStyled(type, folder, name, isDirectory));
        insertRow.setNextColumn(folder);
        insertRow.setNextColumn(name);
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("mtime")));
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("ext")));
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("uid")));
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("gid")));
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("perms")));
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("size")));
        rowSetStyled.add(insertRow.getRow());
    }

    private Object getTypeStyled(final Integer type, final String folder, final String name,
                                 final boolean isDirectory) throws UnsupportedEncodingException {
        final String text = toTypeText(type);
        final String href = toHref(request, folder, name, isDirectory);
        return new TableViewLink(text, null, href);
    }

    private static String toTypeText(final Integer value) {
        String text;
        if (value == null) {
            text = null;
        } else if (value.equals(SFTP.S_IFDIR)) {
            text = UTF16.ICON_FOLDER;
        } else if (value.equals(SFTP.S_IFREG)) {
            text = UTF16.ICON_FILE;
        } else if (value.equals(SFTP.S_IFLNK)) {
            text = UTF16.ICON_SYMLINK;
        } else {
            text = null;
        }
        return text;
    }

    private static String toHref(final SFTPRequest request, final String folder, final String name,
                                 final boolean isDirectory) throws UnsupportedEncodingException {
        final boolean fullPath = name.contains(Http.Token.SLASH);  // in case of load from symlink context
        final String folderURI = (fullPath ? request.getBaseURIServer() :
                request.getHttpRequest().getHttpRequest().getResource());
        final String filename = (fullPath ? name : URLCodec.encode(name));
        final String suffix = (isDirectory ? Http.Token.SLASH : "");
        return Value.join("", folderURI, folder, filename, suffix);
    }

    private static void addFooter(final RowSet rowSet, final Bundle bundle) {
        final PropertiesX propertiesX = new PropertiesX(rowSet.getProperties());
        final long files = propertiesX.getLong(SFTPFolder.Const.FILES);
        final long folders = propertiesX.getLong(SFTPFolder.Const.FOLDERS);
        final long symlinks = propertiesX.getLong(SFTPFolder.Const.SYMLINKS);
        final long bytes = propertiesX.getLong(SFTPFolder.Const.BYTES);
        final String footer = bundle.format("table.sftpFolderType.footer", files, folders, symlinks, bytes);
        rowSet.getProperties().setProperty(Table.Const.FOOTER_C, footer);
    }
}
