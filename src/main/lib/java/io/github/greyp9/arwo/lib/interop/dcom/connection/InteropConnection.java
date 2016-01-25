package io.github.greyp9.arwo.lib.interop.dcom.connection;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.util.PropertiesX;

import java.util.Date;
import java.util.Properties;

public class InteropConnection {
    private final long dateOpen;
    private final Properties properties;
    private final PropertiesX propertiesX;

    public final Date getDateOpen() {
        return new Date(dateOpen);
    }

    public final Properties getProperties() {
        return properties;
    }

    public InteropConnection(final String host, final String user, final String password) {
        this.dateOpen = new Date().getTime();
        this.properties = new Properties();
        properties.setProperty(App.Settings.HOST, host);
        properties.setProperty(App.Settings.USER , user);
        properties.setProperty(App.Settings.PASSWORD, password);
        this.propertiesX = new PropertiesX(properties);
        propertiesX.setLong(App.Connection.DATE_LAST, dateOpen);
        propertiesX.setLong(App.Connection.COUNT, 0L);
        propertiesX.setLong(App.Connection.MILLIS, 0L);
    }

    public final String getHost() {
        return properties.getProperty(App.Settings.HOST);
    }

    public final String getUser() {
        return properties.getProperty(App.Settings.USER);
    }

    public final String getPassword() {
        return properties.getProperty(App.Settings.PASSWORD);
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
}
