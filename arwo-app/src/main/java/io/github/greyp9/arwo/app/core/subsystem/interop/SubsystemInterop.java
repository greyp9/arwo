package io.github.greyp9.arwo.app.core.subsystem.interop;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.io.script.History;

import java.util.Properties;

public class SubsystemInterop {
    // connection entries
    private final ConnectionCache cache;
    // usage history
    private final History history;
    // properties
    private final Properties properties;

    public final ConnectionCache getCache() {
        return cache;
    }

    public final History getHistory() {
        return history;
    }

    public final Properties getProperties() {
        return properties;
    }

    public SubsystemInterop(final Alerts alerts) {
        this.cache = new ConnectionCache(App.Cache.WSH, alerts);
        this.history = new History();
        this.properties = new Properties();
    }
}
