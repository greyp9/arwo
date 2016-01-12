package io.github.greyp9.arwo.core.jdbc.runnable;

import io.github.greyp9.arwo.core.jdbc.connection.JDBCConnection;

public class QueryContext {
    private final JDBCConnection connection;

    public JDBCConnection getConnection() {
        return connection;
    }

    public QueryContext(JDBCConnection connection) {
        this.connection = connection;
    }
}
