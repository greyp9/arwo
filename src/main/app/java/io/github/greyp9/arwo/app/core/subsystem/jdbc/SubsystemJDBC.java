package io.github.greyp9.arwo.app.core.subsystem.jdbc;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.jdbc.query.History;

import java.util.Properties;

public class SubsystemJDBC {
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

    public SubsystemJDBC(final Alerts alerts) {
        this.cache = new ConnectionCache(App.Cache.JDBC, alerts);
        this.history = new History();
        this.properties = new Properties();
    }
}
