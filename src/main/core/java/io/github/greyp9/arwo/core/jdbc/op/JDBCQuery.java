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

    public JDBCQuery(final Connection connection, final Results results/*, BlobCache blobCache*/) {
        this.connection = connection;
        this.results = results;
        //this.blobCache = blobCache;
    }

    public final Results execute(final String sql) throws SQLException, IOException {
        final Statement statement = connection.createStatement();
        try {
            results.getInterval().setDateStart(new Date());
            return execute(statement, sql);
        } finally {
            results.getInterval().setDateFinish(new Date());
            statement.close();
        }
    }

    private Results execute(final Statement statement, final String sql) throws SQLException, IOException {
        boolean moreResults = true;
        boolean isResultSet = statement.execute(sql);
        while (moreResults) {
            moreResults = (isResultSet ? getResultSet(statement) : getUpdateCount(statement));
            isResultSet = (moreResults && statement.getMoreResults());
        }
        return results;
    }

    @SuppressWarnings({ "PMD.AvoidInstantiatingObjectsInLoops", "PMD.CloseResource" })
    private boolean getResultSet(final Statement statement) throws SQLException, IOException {
        final ResultSet resultSet = statement.getResultSet();
        final ResultSetMetaData resultSetMetaData = resultSet.getMetaData();
        final int columnCount = resultSetMetaData.getColumnCount();
        final RowSetMetaData metaData = getMetaData(resultSet);
        final RowSet rowSet = new RowSet(metaData, null, null);
        while (resultSet.next()) {
            final InsertRow insertRow = new InsertRow(rowSet);
            for (int i = 1; (i <= columnCount); ++i) {
                final Object object = resultSet.getObject(i);
                final int type = resultSetMetaData.getColumnType(i);
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

    private boolean getUpdateCount(final Statement statement) throws SQLException {
        final int updateCount = statement.getUpdateCount();
        final boolean isUpdateCount = (updateCount != -1);
        if (isUpdateCount) {
            results.add("jdbcUpdateCountType", null, Integer.toString(updateCount));
        }
        return isUpdateCount;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private RowSetMetaData getMetaData(final ResultSet resultSet) throws SQLException {
        final ResultSetMetaData resultSetMetaData = resultSet.getMetaData();
        final int columnCount = resultSetMetaData.getColumnCount();
        final ColumnMetaData[] columns = new ColumnMetaData[columnCount];
        for (int i = 1; (i <= columnCount); ++i) {
            final String name = resultSetMetaData.getColumnName(i);
            final int type = resultSetMetaData.getColumnType(i);
            columns[i - 1] = new ColumnMetaData(name, type);
        }
        return new RowSetMetaData("jdbcResultSetType", columns);
    }

    private Object doClob(final Clob clob) throws SQLException, IOException {
        final String string = ReaderU.read(clob.getCharacterStream());
        return doBytes(UTF8Codec.toBytes(string), "CLOB");
    }

    private Object doBlob(final Blob blob) throws SQLException, IOException {
        final byte[] bytes = StreamU.read(blob.getBinaryStream());  // pull blob back from server
        return doBytes(bytes, "BLOB");
    }

    private Object doVarBinary(final byte[] bytes) throws SQLException, IOException {
        return doBytes(bytes, "VARBINARY");
    }

    private Object doBytes(final byte[] bytes, final String sqlType) throws SQLException, IOException {
        // cache blob locally
        //Http.Token.SLASH + Long.toHexString(CRCU.crc32(bytes));
        //blobCache.put(new Blob(hash, bytes));
        // insert link into result set
        final String title = String.format("%s [%sB]", sqlType, NumberScale.toString(bytes.length));
        //String href = blobCache.getEndpoint() + hash;
        return new TableViewLink(UTF16.DOCUMENT_BRACKETS, title, "href");
    }
}
