package io.github.greyp9.arwo.lib.mail.imap.connection;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.util.PropertiesX;

import javax.mail.MessagingException;
import javax.mail.Store;
import java.util.Date;
import java.util.Properties;

public class IMAPConnection {
    private final Store store;
    private final long dateOpen;
    private final Properties properties;
    private final PropertiesX propertiesX;

    public final Store getStore() {
        return store;
    }

    public final Date getDateOpen() {
        return new Date(dateOpen);
    }

    public final Properties getProperties() {
        return properties;
    }

    public IMAPConnection(final Store store) {
        this.store = store;
        this.dateOpen = new Date().getTime();
        this.properties = new Properties();
        this.propertiesX = new PropertiesX(properties);
        propertiesX.setLong(App.Connection.DATE_LAST, dateOpen);
        propertiesX.setLong(App.Connection.COUNT, 0L);
        propertiesX.setLong(App.Connection.MILLIS, 0L);
    }

    public final Date getDateLast() {
        return new Date(propertiesX.getLong(App.Connection.DATE_LAST));
    }

    public final long getCount() {
        return propertiesX.getLong(App.Connection.COUNT);
    }

    public final long getMillis() {
        return propertiesX.getLong(App.Connection.MILLIS);
    }

    public final void update(final Date date) {
        propertiesX.setLong(App.Connection.DATE_LAST, date.getTime());
        propertiesX.addLong(App.Connection.COUNT, 1L);
        final long millis = SystemU.currentTimeMillis() - date.getTime();
        propertiesX.addLong(App.Connection.MILLIS, millis);
    }

    public final void close() {
        try {
            store.close();
        } catch (MessagingException e) {
            properties.setProperty(e.getClass().getName(), e.getMessage());
        }
    }
}
