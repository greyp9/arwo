package io.github.greyp9.arwo.app.local.fs.data;

import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.FileX;
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
import java.util.Properties;
import java.util.logging.Logger;

public class LFSFolderStyled {
    private final LFSRequest request;
    private final RowSet rowSet;

    public final RowSet getRowSet() {
        return rowSet;
    }

    public LFSFolderStyled(final LFSRequest request, final RowSet rowSetRaw) throws UnsupportedEncodingException {
        this.request = request;
        this.rowSet = new RowSet(rowSetRaw.getMetaData(), null, null);
        final Properties rsProperties = rowSetRaw.getProperties();
        boolean isSymlinkRowSet = Boolean.parseBoolean(rsProperties.getProperty(Integer.toString(App.FS.S_IFLNK)));
        final Iterator<Row> iterator = rowSetRaw.iterator();
        while (iterator.hasNext()) {
            loadRow(this.rowSet, isSymlinkRowSet, iterator.next());
        }
        this.rowSet.getProperties().putAll(rsProperties);
        addFooter(this.rowSet, request.getBundle());
    }

    private void loadRow(final RowSet rowSetStyled, final boolean isSymlinkRowSet, final Row rowRaw)
            throws UnsupportedEncodingException {
        // input
        final RowSetMetaData metaData = rowSetStyled.getMetaData();
        final Integer type = rowRaw.getInteger(metaData.getIndex("type"));  // i18n metadata
        final String folder = rowRaw.getString(metaData.getIndex("folder"));  // i18n metadata
        final String name = rowRaw.getString(metaData.getIndex("name"));  // i18n metadata
        // processing
        //final boolean isLink = (App.FS.S_IFLNK == NumberU.toInt(type, 0));
        final boolean isDirectory = (App.FS.S_IFDIR == NumberU.toInt(type, 0));
        // output
        final InsertRow insertRow = new InsertRow(rowSetStyled);
        insertRow.setNextColumn(getTypeStyled(type, folder, name, isSymlinkRowSet, isDirectory));
        insertRow.setNextColumn(folder);
        insertRow.setNextColumn(name);
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("mtime")));  // i18n metadata
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("ext")));  // i18n metadata
        insertRow.setNextColumn(rowRaw.getColumn(metaData.getIndex("size")));  // i18n metadata
        rowSetStyled.add(insertRow.getRow());
    }

    private Object getTypeStyled(final Integer type, final String folder, final String name,
                                 final boolean isSymlinkRowSet, final boolean isDirectory)
            throws UnsupportedEncodingException {
        final String text = toTypeText(type);
        final String href = toHref(request, folder, name, isSymlinkRowSet, isDirectory);
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

    private static String toHref(final LFSRequest request, final String folder, final String name,
                                 final boolean isSymlinkRowSet, final boolean isDirectory)
            throws UnsupportedEncodingException {
        Logger.getLogger(LFSFolderStyled.class.getName()).finest("" + isSymlinkRowSet);
        // handle symlink
        final boolean fullPath = name.contains(Http.Token.SLASH);  // in case of load from symlink context
        String folderURI = request.getBaseURIFolder();
        // path components
        final String folderNormalized = (fullPath ? "" : URLCodec.encodePath(new FileX(folder).getPath()));
        final String filename = (fullPath ? name : URLCodec.encode(name));
        final String suffix = (isDirectory ? Http.Token.SLASH : "");
        // zip link
        final String zipLinkSuffix = FileTypeU.isZip(name) ? "!/" : "";
        // assemble
        final String hrefRaw = Value.join(
                "", folderURI, Http.Token.SLASH, folderNormalized, filename, suffix, zipLinkSuffix);
        return hrefRaw.replace(Http.Token.SLASH + Http.Token.SLASH, Http.Token.SLASH);
    }

    private static void addFooter(final RowSet rowSet, final Bundle bundle) {
        final PropertiesX propertiesX = new PropertiesX(rowSet.getProperties());
        final long files = propertiesX.getLong(App.FS.FILES);
        final long folders = propertiesX.getLong(App.FS.FOLDERS);
        //final long symlinks = propertiesX.getLong(SFTPFolder.Const.SYMLINKS);
        final long bytes = propertiesX.getLong(App.FS.BYTES);
        final String footer = bundle.format("table.sftpFolderType.footer", files, folders, 0L, bytes);
        rowSet.getProperties().setProperty(Table.Const.FOOTER_C, footer);
    }
}
