package io.github.greyp9.arwo.app.mail.pop3.data;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.model.ExceptionModel;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.sort.Sort;
import io.github.greyp9.arwo.core.table.sort.Sorts;
import io.github.greyp9.arwo.lib.mail.pop3.connection.POP3Connection;

import javax.mail.Address;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Store;
import javax.mail.internet.MimeMessage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.sql.Types;
import java.util.Date;
import java.util.Enumeration;

public class POP3DataSource {
    private final POP3Connection connection;
    private final Alerts alerts;

    public POP3DataSource(final POP3Connection connection, final Alerts alerts) {
        this.connection = connection;
        this.alerts = alerts;
    }

    public static RowSetMetaData getFoldersMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData("select", Types.VARCHAR),
                new ColumnMetaData("name", Types.VARCHAR, true),
                new ColumnMetaData("fullName", Types.VARCHAR),
                new ColumnMetaData("type", Types.INTEGER),
                new ColumnMetaData("messages", Types.INTEGER),
                new ColumnMetaData("new", Types.INTEGER),
                new ColumnMetaData("unread", Types.INTEGER),
        };
        return new RowSetMetaData("pop3FoldersType", columns);
    }

    public static RowSetMetaData getMessagesMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData("select", Types.VARCHAR),
                new ColumnMetaData("message", Types.INTEGER, true),
                new ColumnMetaData("from", Types.VARCHAR),
                new ColumnMetaData("to", Types.VARCHAR),
                new ColumnMetaData("subject", Types.VARCHAR),
                new ColumnMetaData("sent", Types.TIMESTAMP),
                new ColumnMetaData("recv", Types.TIMESTAMP),
                new ColumnMetaData("size", Types.INTEGER),
        };
        return new RowSetMetaData("pop3MessagesType", columns);
    }

    public final RowSet getFolders(final RowSetMetaData metaData) {
        final RowSet rowSet = new RowSet(metaData, null, null);
        final Date date = new Date();
        try {
            final Store store = connection.getStore();
            final Folder[] folders = store.getDefaultFolder().list();
            for (final Folder folder : folders) {
                final InsertRow insertRow = InsertRow.create(rowSet);
                insertRow.setNextColumn(null);
                insertRow.setNextColumn(folder.getName());
                insertRow.setNextColumn(folder.getFullName());
                insertRow.setNextColumn(folder.getType());
                insertRow.setNextColumn(folder.getMessageCount());
                insertRow.setNextColumn(folder.getNewMessageCount());
                insertRow.setNextColumn(folder.getUnreadMessageCount());
                rowSet.add(insertRow.getRow());
            }
        } catch (MessagingException e) {
            new ExceptionModel(alerts).service(new IOException(e), Alert.Severity.ERR);
        }
        connection.update(date);
        return rowSet;
    }

    public final RowSet getMessages(final RowSetMetaData metaData, final String folderName) {
        // "native" sort, in case none supplied by user
        final Sorts sorts = new Sorts(new Sort("recv", false), new Sort("sent", false));  // i18n
        final RowSet rowSet = new RowSet(metaData, sorts, null);
        final Date date = new Date();
        try {
            final Store store = connection.getStore();
            final Folder folder = store.getFolder(folderName);
            folder.open(Folder.READ_ONLY);
            final int messageCount = folder.getMessageCount();
            final int firstMessage = Math.max(1, (messageCount - (Const.FETCH_PAGE_SIZE - 1)));
            final Message[] messages = folder.getMessages(firstMessage, messageCount);
            for (final Message message : messages) {
                final Address[] from = message.getFrom();
                final Address[] to = message.getRecipients(Message.RecipientType.TO);
                final InsertRow insertRow = InsertRow.create(rowSet);
                insertRow.setNextColumn(null);
                insertRow.setNextColumn(message.getMessageNumber());
                insertRow.setNextColumn(POP3DataSourceU.toDisplayString(from));
                insertRow.setNextColumn(POP3DataSourceU.toDisplayString(to));
                insertRow.setNextColumn(message.getSubject());
                insertRow.setNextColumn(message.getSentDate());
                insertRow.setNextColumn(message.getReceivedDate());
                insertRow.setNextColumn(message.getSize());
                rowSet.add(insertRow.getRow());
            }
            folder.close(false);
        } catch (MessagingException e) {
            new ExceptionModel(alerts).service(new IOException(e), Alert.Severity.ERR);
        }
        connection.update(date);
        rowSet.updateOrdinals(0);
        return rowSet;
    }

    public final MetaFile getMessage(final String folderName, final String messageNumber) {
        final StringBuilder buffer = new StringBuilder();
        final Date date = new Date();
        try {
            final Store store = connection.getStore();
            final Folder folder = store.getFolder(folderName);
            folder.open(Folder.READ_ONLY);
            final Message message = folder.getMessage(NumberU.toInt(messageNumber, 0));
            if (message instanceof MimeMessage) {
                final MimeMessage mimeMessage = (MimeMessage) message;
                final Enumeration<?> headerLines = mimeMessage.getAllHeaderLines();
                while (headerLines.hasMoreElements()) {
                    final String header = (String) headerLines.nextElement();
                    buffer.append(header);
                    buffer.append(Http.Token.CRLF);
                }
                buffer.append(Http.Token.CRLF);
                final byte[] body = StreamU.read(mimeMessage.getRawInputStream());
                buffer.append(UTF8Codec.toString(body));
            }
            folder.close(false);
        } catch (MessagingException e) {
            new ExceptionModel(alerts).service(new IOException(e), Alert.Severity.ERR);
        } catch (IOException e) {
            new ExceptionModel(alerts).service(new IOException(e), Alert.Severity.ERR);
        }
        connection.update(date);
        return new MetaFile(null, new ByteArrayInputStream(UTF8Codec.toBytes(buffer.toString())));
    }

    private static class Const {
        private static final int FETCH_PAGE_SIZE = 30;
    }
}
