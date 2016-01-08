package io.github.greyp9.arwo.app.core.subsystem.cifs;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.connect.ConnectionCache;

import java.util.Properties;

public class SubsystemCIFS {
    // connection entries
    private final ConnectionCache cache;
    // properties
    private final Properties properties;

    public final ConnectionCache getCache() {
        return cache;
    }

    public final Properties getProperties() {
        return properties;
    }

    public SubsystemCIFS(final Alerts alerts) {
        this.cache = new ConnectionCache("cifs", alerts);
        this.properties = new Properties();
    }
}
