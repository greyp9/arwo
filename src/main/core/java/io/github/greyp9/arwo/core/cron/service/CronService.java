package io.github.greyp9.arwo.core.cron.service;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cron.exec.CronTabExecutor;
import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.cron.rowset.tab.CronTabRowSet;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.vm.mutex.CollectionU;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;

import java.io.IOException;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

@SuppressWarnings("PMD.DoNotUseThreads")
public class CronService implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final String context;
    private final Collection<CronTabExecutor> executors;
    private final AtomicReference<String> stopCondition;

    public CronService(final String context) {
        this.context = context;
        this.executors = new ArrayList<CronTabExecutor>();
        this.stopCondition = new AtomicReference<String>();
    }

    public final RowSet createExecutorRowSet(final Principal principal, final Date dateNow) {
        return new CronTabRowSet(getExecutors(), dateNow).getRowSet(principal, "cronTabExecutorType");
    }

    public final Collection<CronTabExecutor> getExecutors() {
        return CollectionU.copy(new ArrayList<CronTabExecutor>(), executors);
    }

    public final void stop(final String condition) {
        this.stopCondition.set(condition);
    }

    public final void runNow(final Principal principal, final Date dateNow,
                             final String tabName, final String jobName) throws IOException {
        synchronized (executors) {
            for (final CronTabExecutor executor : executors) {
                final String tabNameIt = executor.getCronTab().getName();
                final String principalNameIt = executor.getPrincipal().getName();
                final boolean isTab = tabNameIt.equals(tabName);
                final boolean isPrincipal = principalNameIt.equals(principal.getName());
                final boolean isRunning = (!executor.isStopped());
                if (isTab && isPrincipal && isRunning) {
                    for (final CronJob cronJob : executor.getCronTab().getJobs()) {
                        if (cronJob.getName().equals(jobName)) {
                            executor.doWork(context, dateNow, cronJob);
                        }
                    }
                }
            }
        }
    }

    public final void add(final CronTabExecutor executor, final Bundle bundle, final Alerts alerts) {
        synchronized (executors) {
            final String tabNameIt = executor.getCronTab().getName();
            final String principalNameIt = executor.getPrincipal().getName();
            executors.add(executor);
            logger.info(String.format("ADD/%s/%s", tabNameIt, principalNameIt));
            if ((bundle != null) && (alerts != null)) {
                final String message = bundle.format("CronService.cronTab.on", tabNameIt, principalNameIt);
                alerts.add(new Alert(Alert.Severity.INFO, message));
            }
        }
        MutexU.notifyAll(this);
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public final void remove(final String principalName, final Date date, final Bundle bundle, final Alerts alerts) {
        synchronized (executors) {
            for (final CronTabExecutor executor : executors) {
                final String tabNameIt = executor.getCronTab().getName();
                final String principalNameIt = executor.getPrincipal().getName();
                final boolean isPrincipal = principalNameIt.equals(principalName);
                final boolean isRunning = (!executor.isStopped());
                if (isPrincipal && isRunning) {
                    executor.stop(date);
                    logger.info(String.format("REMOVE/%s/%s", tabNameIt, principalNameIt));
                    if ((bundle != null) && (alerts != null)) {
                        final String message = bundle.format("CronService.cronTab.off", tabNameIt, principalNameIt);
                        alerts.add(new Alert(Alert.Severity.INFO, message));
                    }
                }
            }
        }
    }

    @Override
    public final void run() {
        logger.info(String.format("%s/START", context));
        try {
            runWorkLoop();
            cleanUpState();
        } catch (IOException e) {
            logger.severe(String.format("%s/%s", context, e.getMessage()));
        } finally {
            logger.info(String.format("%s/STOP", context));
        }
    }

    private void runWorkLoop() throws IOException {
        Date dateNext = getNextTime(new Date(), Const.DURATION_WORKLOOP);
        while (stopCondition.get() == null) {
            logger.finest(String.format("%s/NEXT/%s", context, XsdDateU.toXSDZMillis(dateNext)));
            MutexU.waitUntil(this, dateNext);
            if (stopCondition.get() == null) {
                dateNext = doWork(dateNext);
            }
        }
    }

    private void cleanUpState() {
        // clean up any active executors
        synchronized (executors) {
            final Date date = new Date();
            for (final CronTabExecutor executor : executors) {
                final String principalNameIt = executor.getPrincipal().getName();
                remove(principalNameIt, date, null, null);
            }
        }
    }

    private Date getNextTime(final Date date, final String duration) {
        Date dateNext = DurationU.add(date, DateU.Const.TZ_GMT, duration);
        synchronized (executors) {
            for (final CronTabExecutor executor : executors) {
                dateNext = DateU.min(dateNext, executor.getDateNext(date, duration));
            }
        }
        return dateNext;
    }

    private Date doWork(final Date dateSchedule) throws IOException {
        final Date dateNow = DurationU.add(new Date(), DateU.Const.TZ_GMT, Const.FUZZ_WORKLOOP);
        Date dateNext;
        if (dateNow.after(dateSchedule)) {
            doWorkIt(dateSchedule);
            dateNext = getNextTime(dateSchedule, Const.DURATION_WORKLOOP);
        } else {
            dateNext = getNextTime(dateNow, Const.DURATION_WORKLOOP);
        }
        return dateNext;
    }

    private void doWorkIt(final Date dateSchedule) throws IOException {
        synchronized (executors) {
            for (final CronTabExecutor executor : executors) {
                if (!executor.isStopped()) {
                    executor.doWork(context, dateSchedule);
                }
            }
        }
    }

    private static class Const {
        private static final String DURATION_WORKLOOP = "PT1M";
        private static final String FUZZ_WORKLOOP = "PT1S";
    }
}
