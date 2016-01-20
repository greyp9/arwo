package io.github.greyp9.arwo.core.jdbc.runnable;

import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.jdbc.connection.JDBCConnection;

import java.io.File;

public class QueryContext {
    private final JDBCConnection connection;
    private final ResourceCache cacheBlob;
    private final File file;

    public final JDBCConnection getConnection() {
        return connection;
    }

    public final ResourceCache getCacheBlob() {
        return cacheBlob;
    }

    public final File getFile() {
        return file;
    }

    public QueryContext(final JDBCConnection connection, final ResourceCache cacheBlob, final File file) {
        this.connection = connection;
        this.cacheBlob = cacheBlob;
        this.file = file;
    }
}
