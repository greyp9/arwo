package io.github.greyp9.arwo.core.jdbc.op;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.io.ReaderU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.number.NumberScale;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;

import java.io.IOException;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.Date;

public class JDBCQuery {
    private final Connection connection;
    private final Results results;
    //private final BlobCache blobCache;

    public JDBCQuery(Connection connection, Results results/*, BlobCache blobCache*/) {
        this.connection = connection;
        this.results = results;
        //this.blobCache = blobCache;
    }

    public Results execute(String sql) throws SQLException, IOException {
        Statement statement = connection.createStatement();
        try {
            return execute(statement, sql);
        } finally {
            statement.close();
        }
    }

    private Results execute(Statement statement, String sql) throws SQLException, IOException {
        results.getInterval().setDateStart(new Date());
        boolean moreResults = true;
        boolean isResultSet = statement.execute(sql);
        while (moreResults) {
            moreResults = (isResultSet ? getResultSet(statement, results) : getUpdateCount(statement, results));
            isResultSet = (moreResults && statement.getMoreResults());
        }
        results.getInterval().setDateFinish(new Date());
        return results;
    }

    private boolean getResultSet(Statement statement, Results results) throws SQLException, IOException {
        ResultSet resultSet = statement.getResultSet();
        ResultSetMetaData resultSetMetaData = resultSet.getMetaData();
        int columnCount = resultSetMetaData.getColumnCount();
        RowSetMetaData metaData = getMetaData(resultSet);
        RowSet rowSet = new RowSet(metaData, null, null);
        while (resultSet.next()) {
            InsertRow insertRow = new InsertRow(rowSet);
            for (int i = 1; (i <= columnCount); ++i) {
                Object object = resultSet.getObject(i);
                int type = resultSetMetaData.getColumnType(i);
                //String columnName = resultSetMetaData.getColumnName(i);
                //String columnLabel = resultSetMetaData.getColumnLabel(i);
                if (object == null) {
                    insertRow.setNextColumn(null);
                } else if (type == Types.CLOB) {
                    insertRow.setNextColumn(doClob((Clob) object));
                } else if (type == Types.BLOB) {
                    insertRow.setNextColumn(doBlob((Blob) object));
                } else if (type == Types.VARBINARY) {
                    insertRow.setNextColumn(doVarBinary((byte[]) object));
                } else {
                    insertRow.setNextColumn(object);
                }
            }
            rowSet.add(insertRow.getRow());
        }
        results.add("rs", null, rowSet);
        return true;
    }

    private boolean getUpdateCount(Statement statement, Results results) throws SQLException {
        int updateCount = statement.getUpdateCount();
        boolean isUpdateCount = (updateCount != -1);
        if (isUpdateCount) {
            results.add("jdbcUpdateCountType", null, Integer.toString(updateCount));
        }
        return isUpdateCount;
    }

    private RowSetMetaData getMetaData(ResultSet resultSet) throws SQLException {
        ResultSetMetaData resultSetMetaData = resultSet.getMetaData();
        int columnCount = resultSetMetaData.getColumnCount();
        ColumnMetaData[] columns = new ColumnMetaData[columnCount];
        for (int i = 1; (i <= columnCount); ++i) {
            String name = resultSetMetaData.getColumnName(i);
            int type = resultSetMetaData.getColumnType(i);
            columns[i - 1] = new ColumnMetaData(name, type);
        }
        return new RowSetMetaData("jdbcResultSetType", columns);
    }

    private Object doClob(Clob clob) throws SQLException, IOException {
        String string = ReaderU.read(clob.getCharacterStream());
        return doBytes(UTF8Codec.toBytes(string), "CLOB");
    }

    private Object doBlob(Blob blob) throws SQLException, IOException {
        byte[] bytes = StreamU.read(blob.getBinaryStream());  // pull blob back from server
        return doBytes(bytes, "BLOB");
    }

    private Object doVarBinary(byte[] bytes) throws SQLException, IOException {
        return doBytes(bytes, "VARBINARY");
    }

    private Object doBytes(byte[] bytes, String sqlType) throws SQLException, IOException {
        // cache blob locally
        //String hash = Http.Token.SLASH + Long.toHexString(CRCU.crc32(bytes));
        //blobCache.put(new Blob(hash, bytes));
        // insert link into result set
        String title = String.format("%s [%sB]", sqlType, NumberScale.toString(bytes.length));
        //String href = blobCache.getEndpoint() + hash;
        String href = "href";
        return new TableViewLink(UTF16.DOCUMENT_BRACKETS, title, href);
    }
}
