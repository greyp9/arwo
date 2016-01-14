package io.github.greyp9.arwo.core.jdbc.runnable;

import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.jdbc.connection.JDBCConnection;

import java.io.File;

public class QueryContext {
    private final JDBCConnection connection;
    private final ResourceCache cacheBlob;
    private final File folder;

    public final JDBCConnection getConnection() {
        return connection;
    }

    public final ResourceCache getCacheBlob() {
        return cacheBlob;
    }

    public final File getFolder() {
        return folder;
    }

    public QueryContext(final JDBCConnection connection, final ResourceCache cacheBlob, final File folder) {
        this.connection = connection;
        this.cacheBlob = cacheBlob;
        this.folder = folder;
    }
}
