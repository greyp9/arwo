package io.github.greyp9.arwo.core.jdbc.runnable;

import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.jdbc.connection.JDBCConnection;

public class QueryContext {
    private final JDBCConnection connection;
    private final ResourceCache cacheBlob;

    public final JDBCConnection getConnection() {
        return connection;
    }

    public final ResourceCache getCacheBlob() {
        return cacheBlob;
    }

    public QueryContext(final JDBCConnection connection, final ResourceCache cacheBlob) {
        this.connection = connection;
        this.cacheBlob = cacheBlob;
    }
}
