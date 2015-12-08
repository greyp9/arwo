package io.github.greyp9.arwo.lib.ganymed.ssh.connection;

import ch.ethz.ssh2.Connection;
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
        propertiesX.setLong("dateLast", dateOpen);
        propertiesX.setLong("count", 0L);
        propertiesX.setLong("millis", 0L);
    }

    public final Date getDateLast() {
        return new Date(propertiesX.getLong("dateLast"));
    }

    public final long getCount() {
        return propertiesX.getLong("count");
    }

    public final long getMillis() {
        return propertiesX.getLong("millis");
    }

    public final void update(final Date date) {
        propertiesX.setLong("dateLast", date.getTime());
        propertiesX.addLong("count", 1L);
        final long millis = SystemU.currentTimeMillis() - date.getTime();
        propertiesX.addLong("millis", millis);
    }
}
