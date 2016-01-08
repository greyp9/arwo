package io.github.greyp9.arwo.app.core.subsystem.dav;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.connect.ConnectionCache;

import java.util.Properties;

public class SubsystemWebDAV {
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

    public SubsystemWebDAV(final Alerts alerts) {
        this.cache = new ConnectionCache(App.Cache.WEBDAV, alerts);
        this.properties = new Properties();
    }
}
