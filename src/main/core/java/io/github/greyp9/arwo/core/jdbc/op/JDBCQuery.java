package io.github.greyp9.arwo.core.jdbc.op;

import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.io.ReaderU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.number.NumberScale;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;

import java.io.ByteArrayInputStream;
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
    private final ResourceCache cacheBlob;

    public JDBCQuery(final Connection connection, final Results results, final ResourceCache cacheBlob) {
        this.connection = connection;
        this.results = results;
        this.cacheBlob = cacheBlob;
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

    @SuppressWarnings({ "PMD.CloseResource" })
    private boolean getResultSet(final Statement statement) throws SQLException, IOException {
        final ResultSet resultSet = statement.getResultSet();
        final ResultSetMetaData resultSetMetaData = resultSet.getMetaData();
        final int columnCount = resultSetMetaData.getColumnCount();
        final RowSetMetaData metaData = getMetaData(resultSet);
        final RowSet rowSet = new RowSet(metaData, null, null);
        while (resultSet.next()) {
            final InsertRow insertRow = InsertRow.create(rowSet);
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
        results.add(Const.ID_RESULT_SET, null, rowSet);
        return true;
    }

    private boolean getUpdateCount(final Statement statement) throws SQLException {
        final int updateCount = statement.getUpdateCount();
        final boolean isUpdateCount = (updateCount != -1);
        if (isUpdateCount) {
            results.add(Const.UPDATE_COUNT_TYPE, null, Integer.toString(updateCount));
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
        return new RowSetMetaData(Const.RESULT_SET_TYPE, columns);
    }

    private Object doClob(final Clob clob) throws SQLException, IOException {
        final String string = ReaderU.read(clob.getCharacterStream());
        return doBytes(UTF8Codec.toBytes(string), Const.CLOB);
    }

    private Object doBlob(final Blob blob) throws SQLException, IOException {
        final byte[] bytes = StreamU.read(blob.getBinaryStream());  // pull blob back from server
        return doBytes(bytes, Const.BLOB);
    }

    private Object doVarBinary(final byte[] bytes) throws SQLException, IOException {
        return doBytes(bytes, Const.VARBINARY);
    }

    private Object doBytes(final byte[] bytes, final String sqlType) throws SQLException, IOException {
        // cache blob locally
        final String resource = Http.Token.SLASH + Long.toHexString(CRCU.crc32(bytes));
        cacheBlob.putFile(resource, new MetaFile(null, null, new ByteArrayInputStream(bytes)));
        // insert link into result set
        final String title = String.format("%s [%sB]", sqlType, NumberScale.toString(bytes.length));
        final String href = cacheBlob.getEndpoint() + resource;
        return new TableViewLink(UTF16.DOCUMENT_BRACKETS, title, href);
    }

    private static class Const {
        private static final String ID_RESULT_SET = "resultSet";  // i18n internal
        private static final String RESULT_SET_TYPE = "jdbcResultSetType";  // i18n internal
        private static final String UPDATE_COUNT_TYPE = "jdbcUpdateCountType";  // i18n internal

        private static final String BLOB = "BLOB";  // i18n internal
        private static final String CLOB = "CLOB";  // i18n internal
        private static final String VARBINARY = "VARBINARY";  // i18n internal
    }
}
