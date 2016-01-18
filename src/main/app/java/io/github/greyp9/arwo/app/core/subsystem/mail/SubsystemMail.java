package io.github.greyp9.arwo.app.core.subsystem.mail;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.connect.ConnectionCache;

import java.util.Properties;

public class SubsystemMail {
    // connection entries
    private final ConnectionCache cacheSMTP;
    private final ConnectionCache cacheIMAP;
    // properties
    private final Properties properties;

    public final ConnectionCache getCacheSMTP() {
        return cacheSMTP;
    }

    public final ConnectionCache getCacheIMAP() {
        return cacheIMAP;
    }

    public final Properties getProperties() {
        return properties;
    }

    public SubsystemMail(final Alerts alerts) {
        this.cacheSMTP = new ConnectionCache(App.Cache.SMTP, alerts);
        this.cacheIMAP = new ConnectionCache(App.Cache.IMAP, alerts);
        this.properties = new Properties();
    }
}
