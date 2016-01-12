package io.github.greyp9.arwo.core.jdbc.runnable;

import io.github.greyp9.arwo.core.jdbc.connection.JDBCConnection;
import io.github.greyp9.arwo.core.jdbc.op.JDBCQuery;
import io.github.greyp9.arwo.core.jdbc.query.Query;
import io.github.greyp9.arwo.core.result.op.Results;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

public class QueryRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());
    private final Query query;
    private final QueryContext context;

    public QueryRunnable(Query query, QueryContext context) {
        this.query = query;
        this.context = context;
    }

    @Override
    public void run() {
        try {
            logger.entering(getClass().getName(), Runnable.class.getName());
            query.start();
            runQuery();
        } catch (IOException e) {
            logger.log(Level.SEVERE, e.getMessage(), e);
        } catch (SQLException e) {
            logger.log(Level.SEVERE, e.getMessage(), e);
        } finally {
            query.finish();
        }
    }

    private void runQuery() throws IOException, SQLException {
        final JDBCConnection connection = context.getConnection();
        final Results results = query.getResults();
        results.getInterval().setDateStart(new Date());
        new JDBCQuery(connection.getConnection(), results).execute(query.getText());
        results.getInterval().setDateFinish(new Date());
    }
}
