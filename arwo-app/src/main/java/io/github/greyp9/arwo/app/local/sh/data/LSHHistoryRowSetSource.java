package io.github.greyp9.arwo.app.local.sh.data;

import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.io.script.read.ScriptReader;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import io.github.greyp9.arwo.core.value.Value;

import java.io.File;
import java.io.IOException;
import java.sql.Types;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public final class LSHHistoryRowSetSource implements RowSetSource {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final List<File> files;
    private final ScriptReader scriptReader;

    public LSHHistoryRowSetSource(final List<File> files) {
        this.files = files;
        this.scriptReader = new ScriptReader();
    }

    @Override
    public String getRowSetId() {
        return ROWSET_ID;
    }

    @Override
    public RowSet getRowSet() {
        final Collection<String> commandsSeen = new TreeSet<>();  // only interested in most recent use of command
        final RowSet rowSet = new RowSet(createMetaData(), null, null);
        for (final File file : files) {
            try {
                final Script script = scriptReader.readFrom(file.getName(), StreamU.read(file));
                if (script != null) {
                    final Collection<Command> commands = script.getCommands();
                    final List<String> commandsText = commands.stream()
                            .map(Command::getStdin).collect(Collectors.toList());
                    final String commandText = Value.joinCollection(",", commandsText);
                    Value.doIf(commandsSeen.add(commandText), () -> addRow(rowSet, script, commandText));
                }
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
                new ColumnMetaData(COLUMN_STDIN, Types.VARCHAR),
        };
        return new RowSetMetaData(ROWSET_ID, columns);
    }

    private void addRow(final RowSet rowSet, final Script script, final String commandText) {
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(script.getStart());
        insertRow.setNextColumn(script.getContext());
        insertRow.setNextColumn(commandText);
        rowSet.add(insertRow.getRow());
    }

    public static final String ROWSET_ID = "lshHistoryType";

    private static final String COLUMN_DATE = "dateSubmit";
    private static final String COLUMN_CONTEXT = "context";
    private static final String COLUMN_STDIN = "stdin";
}
