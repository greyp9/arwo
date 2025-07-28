package io.github.greyp9.arwo.app.core.subsystem.s3;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.connect.ConnectionCache;

public class SubsystemS3 {
    // connection entries
    private final ConnectionCache cache;

    public final ConnectionCache getCache() {
        return cache;
    }

    public SubsystemS3(final Alerts alerts) {
        this.cache = new ConnectionCache(App.Cache.S3, alerts);
    }
}
