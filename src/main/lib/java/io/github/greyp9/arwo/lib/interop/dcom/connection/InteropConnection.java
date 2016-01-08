package io.github.greyp9.arwo.lib.interop.dcom.connection;

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
        properties.setProperty(Const.HOST, host);
        properties.setProperty(Const.USER, user);
        properties.setProperty(Const.PASS, password);
        this.propertiesX = new PropertiesX(properties);
        propertiesX.setLong(Const.DATE_LAST, dateOpen);
        propertiesX.setLong(Const.COUNT, 0L);
        propertiesX.setLong(Const.MILLIS, 0L);
    }

    public final String getHost() {
        return properties.getProperty(Const.HOST);
    }

    public final String getUser() {
        return properties.getProperty(Const.USER);
    }

    public final String getPassword() {
        return properties.getProperty(Const.PASS);
    }

    public final Date getDateLast() {
        return new Date(propertiesX.getLong(Const.DATE_LAST));
    }

    public final long getCount() {
        return propertiesX.getLong(Const.COUNT);
    }

    public final long getMillis() {
        return propertiesX.getLong(Const.MILLIS);
    }

    public final void update(final Date date) {
        propertiesX.setLong(Const.DATE_LAST, date.getTime());
        propertiesX.addLong(Const.COUNT, 1L);
        final long millis = SystemU.currentTimeMillis() - date.getTime();
        propertiesX.addLong(Const.MILLIS, millis);
    }

    private static class Const {
        private static final String HOST = "host";  // i18n
        private static final String USER = "user";  // i18n
        private static final String PASS = "pass";  // i18n

        private static final String DATE_LAST = "dateLast";  // i18n
        private static final String COUNT = "count";  // i18n
        private static final String MILLIS = "millis";  // i18n
    }
}
