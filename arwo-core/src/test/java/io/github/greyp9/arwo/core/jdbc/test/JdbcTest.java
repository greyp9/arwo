package io.github.greyp9.arwo.core.jdbc.test;

import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.jdbc.connection.JDBCConnection;
import io.github.greyp9.arwo.core.jdbc.query.Query;
import io.github.greyp9.arwo.core.jdbc.runnable.QueryContext;
import io.github.greyp9.arwo.core.jdbc.runnable.QueryRunnable;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.result.type.Result;
import io.github.greyp9.arwo.core.result.type.rowset.RowSetResult;
import io.github.greyp9.arwo.core.result.type.text.TextResult;
import io.github.greyp9.arwo.core.result.view.ResultsContext;
import io.github.greyp9.arwo.core.security.realm.AppPrincipal;
import io.github.greyp9.arwo.core.util.CollectionU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.exec.ExecutorServiceFactory;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.Date;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.logging.Logger;

public class JdbcTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public void setUp() throws Exception {
    }

    @Test
    public void testServerConnectivity() throws Exception {
        File fileProperties = new File(SystemU.userHome(), ".arwo/test.properties.xml");
        Assumptions.assumeTrue(fileProperties.exists());
        Properties properties = PropertiesU.loadFromXml(fileProperties.toURI().toURL());
        logger.info("" + properties.size());
        Assertions.assertTrue(properties.size() > 0);
        String sshServerList = properties.getProperty(Const.JDBC_SERVER);
        Assertions.assertNotNull(sshServerList);
        String[] servers = sshServerList.split(",");
        for (String server : servers) {
            if (server.length() > 0) {
                doTestConnectivityServer(server, properties);
            }
        }
    }

    private void doTestConnectivityServer(final String server, final Properties properties)
            throws IOException, ClassNotFoundException, SQLException, InstantiationException, IllegalAccessException {
        String driver = properties.getProperty(String.format("%s.%s.driver", Const.JDBC_SERVER, server));
        String url = properties.getProperty(String.format("%s.%s.url", Const.JDBC_SERVER, server));
        String user = properties.getProperty(String.format("%s.%s.user", Const.JDBC_SERVER, server));
        String pass = properties.getProperty(String.format("%s.%s.pass", Const.JDBC_SERVER, server));
        logger.info(String.format("Authenticate: URL=[%s]", url));
        doTestConnectivityServer(driver, url, user, pass, server, properties);
    }

    private void doTestConnectivityServer(final String driverClassName, final String url, final String user,
                                          final String pass, final String server, final Properties propertiesTest)
            throws IOException, ClassNotFoundException, IllegalAccessException, InstantiationException, SQLException {
        Class<?> driverClass = Class.forName(driverClassName);
        Properties properties = new Properties();
        PropertiesU.setProperty(properties, "user", user);
        PropertiesU.setProperty(properties, "password", pass);
        Driver driver = (Driver) driverClass.newInstance();
        Connection connection = driver.connect(url, properties);
        Assertions.assertNotNull(connection);
        int i = 0;
        String sql = propertiesTest.getProperty(String.format("%s.%s.sql.%d", Const.JDBC_SERVER, server, ++i));
        while (sql != null) {
            doTestConnectivityServer(connection, sql);
            doTestConnectivityServerExecutor(connection, sql);
            sql = propertiesTest.getProperty(String.format("%s.%s.sql.%d", Const.JDBC_SERVER, server, ++i));
        }
        connection.close();
    }

    private void doTestConnectivityServer(final Connection connection,
                                          final String sql) throws IOException, SQLException {
        logger.info(sql);
        Statement statement = connection.createStatement();
        try {
            doTestConnectivityServer(statement, sql);
        } finally {
            statement.close();
        }
    }

    private void doTestConnectivityServerExecutor(final Connection connection,
                                                  final String sql) throws IOException, SQLException {
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(os, true, UTF8Codec.Const.UTF8);
        final AppPrincipal principal = new AppPrincipal("root", CollectionU.toCollection("*"));
        final JDBCConnection jdbcConnection = new JDBCConnection(connection);
        final ExecutorService executor = ExecutorServiceFactory.create(
                1, Value.join(Html.HYPHEN, getClass().getSimpleName(), principal.getName()));
        final Query query = new Query(null, new Date().getTime(), null, sql);
        final QueryContext context = new QueryContext(
                jdbcConnection, new ResourceCache(""), ResultsContext.createEmpty());
        executor.execute(new QueryRunnable(query, context));
        final Results results = query.getResults();
        for (Result result : results.getResults()) {
            if (result instanceof TextResult) {
                final TextResult textResult = (TextResult) result;
                ps.println(textResult.getText());
            } else if (result instanceof RowSetResult) {
                final RowSetResult rowSetResult = (RowSetResult) result;
                ps.println(rowSetResult.getRowSet().getRows());
            }
        }
    }

    private void doTestConnectivityServer(final Statement statement,
                                          final String sql) throws IOException, SQLException {
        boolean moreResults = true;
        boolean isResultSet = statement.execute(sql);
        while (moreResults) {
            moreResults = (isResultSet ? getResultSet(statement) : getUpdateCount(statement));
            isResultSet = (moreResults && statement.getMoreResults());
        }
    }

    private boolean getResultSet(final Statement statement) throws IOException, SQLException {
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(os, true, UTF8Codec.Const.UTF8);
        ResultSet resultSet = statement.getResultSet();
        ResultSetMetaData metaData = resultSet.getMetaData();
        int columnCount = metaData.getColumnCount();
        for (int i = 1; (i <= columnCount); ++i) {
            ps.print(metaData.getColumnName(i) + "   ");
        }
        ps.println();
        while (resultSet.next()) {
            for (int i = 1; (i <= columnCount); ++i) {
                Object object = resultSet.getObject(i);
                int type = metaData.getColumnType(i);
                if (object == null) {
                    ps.print("null");
                } else if (type == Types.CLOB) {
                    ps.print("CLOB");
                } else if (type == Types.BLOB) {
                    ps.print("BLOB");
                } else if (type == Types.VARBINARY) {
                    ps.print("VARBINARY");
                } else {
                    ps.print(object.toString());
                }
                ps.print("   ");
            }
            ps.println();
        }
        logger.info("RESULT SET");
        logger.info(UTF8Codec.toString(os.toByteArray()));
        return true;
    }

    private boolean getUpdateCount(final Statement statement) throws SQLException {
        int updateCount = statement.getUpdateCount();
        boolean isUpdateCount = (updateCount != -1);
        if (isUpdateCount) {
            logger.info("UPDATE COUNT");
            logger.info(Integer.toString(updateCount));
        }
        return isUpdateCount;
    }

    private static class Const {
        private static final String JDBC_SERVER = "jdbc.server";
    }
}
