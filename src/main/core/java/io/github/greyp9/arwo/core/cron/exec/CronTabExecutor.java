package io.github.greyp9.arwo.core.cron.exec;

import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.cron.tab.CronTab;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.table.row.RowSet;

import java.io.IOException;
import java.security.Principal;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

@SuppressWarnings("PMD.DoNotUseThreads")
public class CronTabExecutor {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final String authorization;
    private final Principal principal;
    private final ExecutorService executorService;
    private final CronTab cronTab;
    private final Date dateStart;
    private final AtomicReference<Date> dateStop;
    private final RowSet rowSet;

    public final ExecutorService getExecutorService() {
        return executorService;
    }

    public final CronTab getCronTab() {
        return cronTab;
    }

    public final String getAuthorization() {
        return authorization;
    }

    public final Principal getPrincipal() {
        return principal;
    }

    public final Date getDateStart() {
        return DateU.copy(dateStart);
    }

    public final Date getDateStop() {
        return DateU.copy(dateStop.get());
    }

    public final boolean isStopped() {
        return (dateStop.get() != null);
    }

    public final RowSet getRowSet() {
        return rowSet;
    }

    public CronTabExecutor(final String authorization, final Principal principal,
                           final ExecutorService executorService, final CronTab cronTab,
                           final Date dateStart, final RowSet rowSet) {
        this.authorization = authorization;
        this.principal = principal;
        this.executorService = executorService;
        this.cronTab = cronTab;
        this.dateStart = DateU.copy(dateStart);
        this.dateStop = new AtomicReference<Date>();
        this.rowSet = rowSet;
    }

    @SuppressWarnings({ "PMD.GuardLogStatementJavaUtil", "PMD.GuardLogStatement" })
    public final void stop(final Date date) {
        this.dateStop.set(date);
        // stop executor service
        final List<Runnable> runnables = executorService.shutdownNow();
        for (final Runnable runnable : runnables) {
            logger.warning("TERMINATE" + runnable.toString());
        }
    }

    public final Date getDateNext(final Date date, final String duration) {
        return cronTab.getDateNext(date, duration);
    }

    public final void doWork(final String context, final Date dateSchedule) throws IOException {
        new CronTabWork(context, this, null, dateSchedule).doWork();
    }

    public final void doWork(final String context, final Date dateNow, final CronJob cronJob) throws IOException {
        new CronTabWork(context, this, principal, dateNow).doWork(cronJob);
    }
}
