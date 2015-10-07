package io.github.greyp9.arwo.lib.jdbc.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import junit.framework.TestCase;
import org.junit.Assert;

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
import java.util.Properties;
import java.util.logging.Logger;

public class JdbcTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
    }

    public void testServerConnectivity() throws Exception {
        File fileProperties = new File(SystemU.userHome(), ".arwo/test.properties.xml");
        Assert.assertTrue(fileProperties.exists());
        Properties properties = PropertiesU.loadFromXml(fileProperties.toURI().toURL());
        logger.info("" + properties.size());
        Assert.assertTrue(properties.size() > 0);
        String sshServerList = properties.getProperty(Const.JDBC_SERVER);
        Assert.assertNotNull(sshServerList);
        String[] servers = sshServerList.split(",");
        for (String server : servers) {
            if (server.length() > 0) {
                doTestConnectivityServer(server, properties);
            }
        }
    }

    private void doTestConnectivityServer(String server, Properties properties)
            throws IOException, ClassNotFoundException, SQLException, InstantiationException, IllegalAccessException {
        String driver = properties.getProperty(String.format("%s.%s.driver", Const.JDBC_SERVER, server));
        String url = properties.getProperty(String.format("%s.%s.url", Const.JDBC_SERVER, server));
        String user = properties.getProperty(String.format("%s.%s.user", Const.JDBC_SERVER, server));
        String pass = properties.getProperty(String.format("%s.%s.pass", Const.JDBC_SERVER, server));
        logger.info(String.format("Authenticate: URL=[%s]", url));
        doTestConnectivityServer(driver, url, user, pass, server, properties);
    }

    private void doTestConnectivityServer(
            String driverClassName, String url, String user, String pass, String server, Properties propertiesTest)
            throws IOException, ClassNotFoundException, IllegalAccessException, InstantiationException, SQLException {
        Class<?> driverClass = Class.forName(driverClassName);
        Properties properties = new Properties();
        PropertiesU.setProperty(properties, "user", user);
        PropertiesU.setProperty(properties, "password", pass);
        Driver driver = (Driver) driverClass.newInstance();
        Connection connection = driver.connect(url, properties);
        Assert.assertNotNull(connection);
        int i = 0;
        String sql = propertiesTest.getProperty(String.format("%s.%s.sql.%d", Const.JDBC_SERVER, server, ++i));
        while (sql != null) {
            doTestConnectivityServer(connection, sql);
            sql = propertiesTest.getProperty(String.format("%s.%s.sql.%d", Const.JDBC_SERVER, server, ++i));
        }
        connection.close();
    }

    private void doTestConnectivityServer(Connection connection, String sql) throws IOException, SQLException {
        logger.info(sql);
        Statement statement = connection.createStatement();
        try {
            doTestConnectivityServer(statement, sql);
        } finally {
            statement.close();
        }
    }

    private void doTestConnectivityServer(Statement statement, String sql) throws IOException, SQLException {
        boolean moreResults = true;
        boolean isResultSet = statement.execute(sql);
        while (moreResults) {
            moreResults = (isResultSet ? getResultSet(statement) : getUpdateCount(statement));
            isResultSet = (moreResults && statement.getMoreResults());
        }
    }

    private boolean getResultSet(Statement statement) throws IOException, SQLException {
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

    private boolean getUpdateCount(Statement statement) throws SQLException {
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
