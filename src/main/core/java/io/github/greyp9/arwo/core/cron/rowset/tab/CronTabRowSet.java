package io.github.greyp9.arwo.core.cron.rowset.tab;

import io.github.greyp9.arwo.core.cron.exec.CronTabExecutor;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.table.cell.Duration;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.value.Value;

import java.security.Principal;
import java.sql.Types;
import java.util.Collection;
import java.util.Date;

// i18nf
public class CronTabRowSet {
    private final Collection<CronTabExecutor> executors;
    private final Date dateNow;

    public CronTabRowSet(final Collection<CronTabExecutor> executors, final Date dateNow) {
        this.executors = executors;
        this.dateNow = DateU.copy(dateNow);
    }

    public final RowSet getRowSet(final Principal principal, final String id) {
        final RowSetMetaData metaData = createMetaData(id);
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final CronTabExecutor executor : executors) {
            if (matches(executor.getPrincipal(), principal)) {
                addRow(rowSet, executor);
            }
        }
        return rowSet;
    }

    private static RowSetMetaData createMetaData(final String id) {
        final ColumnMetaData[] columns = {
                new ColumnMetaData("user", Types.VARCHAR),
                new ColumnMetaData("tabName", Types.VARCHAR),
                new ColumnMetaData("dateOpen", Types.TIMESTAMP),
                new ColumnMetaData("dateClose", Types.TIMESTAMP),
                new ColumnMetaData("executor", Types.VARCHAR),
                new ColumnMetaData("durationActive", Types.VARCHAR),
        };
        return new RowSetMetaData(id, columns);
    }

    private static boolean matches(final Principal principalIt, final Principal principal) {
        return ((principal == null) || (principal.getName().equals(principalIt.getName())));
    }

    private void addRow(final RowSet rowSet, final CronTabExecutor executor) {
        final Date dateStart = executor.getDateStart();
        final Date dateStop = executor.getDateStop();
        final Date dateStopSafe = Value.defaultOnNull(dateStop, dateNow);
        final String duration = DurationU.duration(dateStart, dateStopSafe);
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(executor.getPrincipal().getName());
        insertRow.setNextColumn(executor.getCronTab().getName());
        insertRow.setNextColumn(dateStart);
        insertRow.setNextColumn(dateStop);
        insertRow.setNextColumn(Integer.toHexString(executor.hashCode()));
        insertRow.setNextColumn(new Duration(DurationU.toMillis(duration)));
        rowSet.add(insertRow.getRow());
    }
}
