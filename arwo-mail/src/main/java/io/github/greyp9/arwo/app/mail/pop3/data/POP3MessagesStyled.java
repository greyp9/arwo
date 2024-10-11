package io.github.greyp9.arwo.app.mail.pop3.data;

import io.github.greyp9.arwo.app.mail.pop3.core.POP3Request;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.row.RowSet;

import java.io.UnsupportedEncodingException;
import java.util.Iterator;

public final class POP3MessagesStyled {
    private final POP3Request request;
    private final RowSet rowSet;

    public RowSet getRowSet() {
        return rowSet;
    }

    public POP3MessagesStyled(final POP3Request request, final RowSet rowSetRaw) throws UnsupportedEncodingException {
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
        insertRow.getRow().setColumn(metaData.getIndex("select"), getLink(rowRaw, metaData));  // i18n metadata
        rowSetStyled.add(insertRow.getRow());
    }

    private TableViewLink getLink(final Row rowRaw, final RowSetMetaData metaData) throws UnsupportedEncodingException {
        final Integer message = rowRaw.getInteger(metaData.getIndex("message"));  // i18n metadata
        final String href = request.getHttpRequest().getURI() + Integer.toString(message) + Http.Token.SLASH;
        return new TableViewLink(UTF16.SELECT, null, href);
    }
}
