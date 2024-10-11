package io.github.greyp9.arwo.app.cifs.data;

import io.github.greyp9.arwo.app.cifs.core.CIFSRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.type.FileTypeU;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.lang.SystemU;
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

public final class CIFSFolderStyled {
    private final CIFSRequest request;
    private final RowSet rowSet;

    public RowSet getRowSet() {
        return rowSet;
    }

    public CIFSFolderStyled(final CIFSRequest request, final RowSet rowSetRaw) throws UnsupportedEncodingException {
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
        final String perms = rowRaw.getString(metaData.getIndex("perms"));  // i18n metadata
        final String nameDisplay = URLCodec.decode(name);
        // processing
        final boolean isDirectory = (App.FS.S_IFDIR == NumberU.toInt(type, 0));
        // output
        final InsertRow insertRow = new InsertRow(rowSetStyled);
        insertRow.setNextColumn(getTypeStyled(type, folder, name, isDirectory));
        insertRow.setNextColumn(folder);
        insertRow.setNextColumn(nameDisplay);
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("mtime")));  // i18n metadata
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("ext")));  // i18n metadata
        insertRow.setNextColumn(null);
        insertRow.setNextColumn(null);
        insertRow.setNextColumn(getPermsStyled(perms));
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("size")));  // i18n metadata
        rowSetStyled.add(insertRow.getRow());
    }

    private Object getTypeStyled(final Integer type, final String folder, final String name,
                                 final boolean isDirectory) throws UnsupportedEncodingException {
        final String text = toTypeText(type);
        final String href = toHref(request, folder, name, isDirectory);
        return new TableViewLink(text, null, href);
    }

    private String getPermsStyled(final String permsRaw) {
        final String key = Value.join(Http.Token.DOT, rowSet.getID(), "perms", permsRaw);  // i18n metadata
        return request.getBundle().getString(key);
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

    @SuppressWarnings("PMD.NPathComplexity")
    private static String toHref(final CIFSRequest request, final String folder, final String name,
                                 final boolean isDirectory) throws UnsupportedEncodingException {
        // path components
        final boolean fullPath = !SystemU.isTrue();  // name.contains(Http.Token.SLASH);
        final String folderURI = (fullPath ? request.getBaseURIServer()
                : request.getHttpRequest().getHttpRequest().getResource());
        final String nameDisplay = (isDirectory ? name.substring(0, name.length() - Http.Token.SLASH.length()) : name);
        final String filename = (fullPath ? name : URLCodec.encode(nameDisplay));
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
