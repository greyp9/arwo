package io.github.greyp9.arwo.app.ssh.sftp.data;

import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.type.FileTypeU;
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

import java.io.UnsupportedEncodingException;
import java.util.Iterator;

public final class SFTPFolderStyled {
    private final SFTPRequest request;
    private final RowSet rowSet;

    public RowSet getRowSet() {
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
        final Integer type = rowRaw.getInteger(metaData.getIndex("type"));  // i18n metadata
        final String folder = rowRaw.getString(metaData.getIndex("folder"));  // i18n metadata
        final String name = rowRaw.getString(metaData.getIndex("name"));  // i18n metadata
        // processing
        final boolean isDirectory = (App.FS.S_IFDIR == NumberU.toInt(type, 0));
        // output
        final InsertRow insertRow = new InsertRow(rowSetStyled);
        insertRow.setNextColumn(getTypeStyled(type, folder, name, isDirectory));
        insertRow.setNextColumn(folder);
        insertRow.setNextColumn(name);
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("mtime")));  // i18n metadata
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("ext")));  // i18n metadata
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("uid")));  // i18n metadata
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("gid")));  // i18n metadata
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("perms")));  // i18n metadata
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("size")));  // i18n metadata
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
        } else if (value.equals(App.FS.S_IFDIR)) {
            text = UTF16.ICON_FOLDER;
        } else if (value.equals(App.FS.S_IFREG)) {
            text = UTF16.ICON_FILE;
        } else if (value.equals(App.FS.S_IFLNK)) {
            text = UTF16.ICON_SYMLINK;
        } else {
            text = null;
        }
        return text;
    }

    private static String toHref(final SFTPRequest request, final String folder, final String name,
                                 final boolean isDirectory) throws UnsupportedEncodingException {
        // handle symlink
        final boolean fullPath = name.contains(Http.Token.SLASH);  // in case of load from symlink context
        // path components
        final String folderURI = (fullPath ? request.getBaseURIServer()
                : request.getHttpRequest().getHttpRequest().getResource());
        final String filename = (fullPath ? name : URLCodec.encode(name));
        final String suffix = (isDirectory ? Http.Token.SLASH : "");
        // zip link
        final String zipLinkSuffix = FileTypeU.isZip(name) ? "!/" : "";
        // assemble
        return Value.join("", folderURI, folder, filename, suffix, zipLinkSuffix);
    }

    private static void addFooter(final RowSet rowSet, final Bundle bundle) {
        final PropertiesX propertiesX = new PropertiesX(rowSet.getProperties());
        final long files = propertiesX.getLong(App.FS.FILES);
        final long folders = propertiesX.getLong(App.FS.FOLDERS);
        final long symlinks = propertiesX.getLong(App.FS.SYMLINKS);
        final long bytes = propertiesX.getLong(App.FS.BYTES);
        final String footer = bundle.format("table.sftpFolderType.footer", files, folders, symlinks, bytes);
        rowSet.getProperties().setProperty(Table.Const.FOOTER_C, footer);
    }
}
