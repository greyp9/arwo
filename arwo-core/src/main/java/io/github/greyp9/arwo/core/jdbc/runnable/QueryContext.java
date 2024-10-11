package io.github.greyp9.arwo.core.jdbc.runnable;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.jdbc.connection.JDBCConnection;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.result.view.ResultsContext;

import java.io.File;

public class QueryContext {
    private final JDBCConnection connection;
    private final ResourceCache cacheBlob;
    private final ResultsContext resultsContext;

    public final JDBCConnection getConnection() {
        return connection;
    }

    public final ResourceCache getCacheBlob() {
        return cacheBlob;
    }

    public final Locus getLocus() {
        return resultsContext.getLocus();
    }

    public final Bundle getBundle() {
        return resultsContext.getBundle();
    }

    public final Alerts getAlerts() {
        return resultsContext.getAlerts();
    }

    public final File getFile() {
        return resultsContext.getMetaLink().getFile();
    }

    public final String getHref() {
        return resultsContext.getMetaLink().getHref();
    }

    public QueryContext(final JDBCConnection connection, final ResourceCache cacheBlob,
                        final ResultsContext resultsContext) {
        this.connection = connection;
        this.cacheBlob = cacheBlob;
        this.resultsContext = resultsContext;
    }
}
