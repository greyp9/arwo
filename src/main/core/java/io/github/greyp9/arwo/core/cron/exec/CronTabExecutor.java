package io.github.greyp9.arwo.core.cron.exec;

import io.github.greyp9.arwo.core.cron.tab.CronTab;

import java.io.IOException;
import java.security.Principal;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

@SuppressWarnings("PMD.DoNotUseThreads")
public class CronTabExecutor {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final Principal principal;
    private final ExecutorService executorService;
    private final CronTab cronTab;
    private final Properties properties;
    private final AtomicReference<Date> stopDate;

    public final ExecutorService getExecutorService() {
        return executorService;
    }

    public final CronTab getCronTab() {
        return cronTab;
    }

    public final Properties getProperties() {
        return properties;
    }

    public final Principal getPrincipal() {
        return principal;
    }

    public final boolean isStopped() {
        return (stopDate.get() != null);
    }

    public CronTabExecutor(final Principal principal, final ExecutorService executorService,
                           final CronTab cronTab, final Properties properties) {
        this.principal = principal;
        this.executorService = executorService;
        this.cronTab = cronTab;
        this.properties = properties;
        this.stopDate = new AtomicReference<Date>();
    }

    @SuppressWarnings({ "PMD.GuardLogStatementJavaUtil", "PMD.GuardLogStatement" })
    public final void stop(final Date date) {
        this.stopDate.set(date);
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
        new CronTabWork(context, this, dateSchedule).doWork();
    }
}
