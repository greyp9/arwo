package io.github.greyp9.arwo.lib.ganymed.ssh.connection;

import ch.ethz.ssh2.Connection;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.util.PropertiesX;

import java.util.Date;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

public class SSHConnection {
    private final Connection connection;
    private final long dateOpen;
    private final Map<Integer, String> uidToName;
    private final Map<Integer, String> gidToName;
    private final Properties properties;
    private final PropertiesX propertiesX;

    public final Connection getConnection() {
        return connection;
    }

    public final Date getDateOpen() {
        return new Date(dateOpen);
    }

    public final Map<Integer, String> getUidToName() {
        return uidToName;
    }

    public final Map<Integer, String> getGidToName() {
        return gidToName;
    }

    public final Properties getProperties() {
        return properties;
    }

    public SSHConnection(final Connection connection) {
        this.connection = connection;
        this.dateOpen = new Date().getTime();
        this.uidToName = new TreeMap<Integer, String>();
        this.gidToName = new TreeMap<Integer, String>();
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
}
