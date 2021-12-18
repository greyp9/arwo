package io.github.greyp9.arwo.app.mail.imap.data;

import io.github.greyp9.arwo.app.mail.imap.core.IMAPRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.url.URLCodec;

import java.io.UnsupportedEncodingException;
import java.util.Iterator;

public class IMAPFoldersStyled {
    private final IMAPRequest request;
    private final RowSet rowSet;
    private final Bundle bundle;

    public final RowSet getRowSet() {
        return rowSet;
    }

    public IMAPFoldersStyled(final IMAPRequest request, final RowSet rowSetRaw) throws UnsupportedEncodingException {
        this.request = request;
        this.rowSet = new RowSet(rowSetRaw.getMetaData(), null, null);
        this.bundle = request.getBundle();
        final Iterator<Row> iterator = rowSetRaw.iterator();
        while (iterator.hasNext()) {
            loadRow(this.rowSet, iterator.next());
        }
        this.rowSet.getProperties().putAll(rowSetRaw.getProperties());
    }

    private void loadRow(final RowSet rowSetStyled, final Row rowRaw) throws UnsupportedEncodingException {
        // input
        final RowSetMetaData metaData = rowSetStyled.getMetaData();
        // output
        final InsertRow insertRow = new InsertRow(rowSetStyled, rowRaw);
        insertRow.getRow().setColumn(metaData.getIndex("select"), getLink(rowRaw, metaData));  // i18n metadata
        insertRow.getRow().setColumn(metaData.getIndex("type"), getType(rowRaw, metaData));  // i18n metadata
        rowSetStyled.add(insertRow.getRow());
    }

    private TableViewLink getLink(final Row rowRaw, final RowSetMetaData metaData) throws UnsupportedEncodingException {
        final String name = rowRaw.getString(metaData.getIndex("name"));  // i18n metadata
        final String href = PathU.toDir(request.getHttpRequest().getBaseURI(),
                request.getServer(), URLCodec.encode(name));
        return new TableViewLink(UTF16.SELECT, null, href);
    }

    private Object getType(final Row rowRaw, final RowSetMetaData metaData) {
        final int type = NumberU.toInt(rowRaw.getInteger(metaData.getIndex("type")), 0);  // i18n
        final boolean canOpen = ((type % 2) != 0);
        return bundle.getString(canOpen
                ? "IMAPFoldersStyled.folderType.canOpen"
                : "IMAPFoldersStyled.folderType.cannotOpen", Integer.toString(type));
    }
}
