package io.github.greyp9.arwo.app.jdbc.sh.data;

import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.result.xml.ResultsReader;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import io.github.greyp9.arwo.core.value.Value;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.sql.Types;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;
import java.util.logging.Logger;

public final class JDBCHistoryRowSetSource implements RowSetSource {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final List<File> files;
    private final ResultsReader reader;

    public JDBCHistoryRowSetSource(final List<File> files) {
        this.files = files;
        this.reader = new ResultsReader();
    }

    @Override
    public String getRowSetId() {
        return ROWSET_ID;
    }

    @Override
    public RowSet getRowSet() {
        final Collection<String> commands = new TreeSet<>();  // only interested in most recent use of command
        final RowSet rowSet = new RowSet(createMetaData(), null, null);
        for (final File file : files) {
            try {
                final MetaFile metaFile = new MetaFile(null, null, new ByteArrayInputStream(StreamU.read(file)));
                final Results results = reader.readFrom(metaFile);
                Value.doIf(commands.add(results.getCommand()), () -> addRow(rowSet, results));
            } catch (IOException e) {
                logger.warning(e.getMessage());
            }
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(COLUMN_DATE, Types.TIMESTAMP),
                new ColumnMetaData(COLUMN_CONTEXT, Types.VARCHAR),
                new ColumnMetaData(COLUMN_SQL, Types.VARCHAR),
        };
        return new RowSetMetaData(ROWSET_ID, columns);
    }

    private void addRow(final RowSet rowSet, final Results results) {
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(results.getInterval().getDateStart());
        insertRow.setNextColumn(results.getContext());
        insertRow.setNextColumn(results.getCommand());
        rowSet.add(insertRow.getRow());
    }

    public static final String ROWSET_ID = "jdbcHistoryType";

    private static final String COLUMN_DATE = "dateSubmit";
    private static final String COLUMN_CONTEXT = "context";
    private static final String COLUMN_SQL = "sql";
}
