package io.github.greyp9.arwo.app.mail.imap.data;

import io.github.greyp9.arwo.app.mail.imap.core.IMAPRequest;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.row.RowSet;

import java.io.UnsupportedEncodingException;
import java.util.Iterator;

public class IMAPMessagesStyled {
    private final IMAPRequest request;
    private final RowSet rowSet;

    public final RowSet getRowSet() {
        return rowSet;
    }

    public IMAPMessagesStyled(final IMAPRequest request, final RowSet rowSetRaw) throws UnsupportedEncodingException {
        this.request = request;
        this.rowSet = new RowSet(rowSetRaw.getMetaData(), null, null);
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
        insertRow.getRow().setColumn(metaData.getIndex("select"), getLink(rowRaw, metaData));  // i18n
        rowSetStyled.add(insertRow.getRow());
    }

    private TableViewLink getLink(final Row rowRaw, final RowSetMetaData metaData) throws UnsupportedEncodingException {
        final Integer message = rowRaw.getInteger(metaData.getIndex("message"));  // i18n
        final String href = request.getHttpRequest().getURI() + Integer.toString(message) + Http.Token.SLASH;
        return new TableViewLink(UTF16.SELECT, null, href);
    }
}
