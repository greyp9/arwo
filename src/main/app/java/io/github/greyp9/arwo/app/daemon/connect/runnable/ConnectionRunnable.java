package io.github.greyp9.arwo.app.daemon.connect.runnable;

import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.connect.ConnectionResource;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;

import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

@SuppressWarnings("PMD.DoNotUseThreads")
public class ConnectionRunnable implements Runnable {
    private final Logger logger;
    private final AppState appState;
    private final AtomicReference<String> reference;

    public ConnectionRunnable(final AppState appState, final AtomicReference<String> reference) {
        this.logger = Logger.getLogger(getClass().getName());
        this.appState = appState;
        this.reference = reference;
    }

    @SuppressWarnings({
            "PMD.AvoidInstantiatingObjectsInLoops", "PMD.GuardLogStatementJavaUtil", "PMD.GuardLogStatement" })
    @Override
    public final void run() {
        logger.entering(getClass().getName(), Runnable.class.getName());
        Date date = new Date();
        while (reference.get() == null) {
            date = getNextTime(date, Const.DURATION);
            logger.finest("pause");
            MutexU.waitUntil(reference, date);
            logger.finest("resume");
            if (reference.get() == null) {
                date = new Date();
                runMonitor(date, Const.DURATION);
            }
        }
        logger.exiting(getClass().getName(), Runnable.class.getName(), reference.get());
    }

    private Date getNextTime(final Date date, final String duration) {
        Date dateNext = DurationU.add(date, DateU.Const.TZ_GMT, duration);
        final Iterator<AppUserState> it = appState.getIterator();
        while (it.hasNext()) {
            dateNext = DateU.min(dateNext, getNextTime(dateNext, duration, it.next()));
        }
        return dateNext;
    }

    private Date getNextTime(final Date dateNext, final String duration, final AppUserState userState) {
        Date dateNextIt = dateNext;
        final ConnectionCache cache = userState.getSSH().getCache();
        for (final ConnectionResource resource : cache.getResources()) {
            final Date dateIdle = DurationU.add(resource.getDateLast(), DateU.Const.TZ_GMT, duration);
            dateNextIt = DateU.min(dateNext, dateIdle);
        }
        return dateNextIt;
    }

    private void runMonitor(final Date date, final String duration) {
        final Iterator<AppUserState> it = appState.getIterator();
        while (it.hasNext()) {
            runMonitorUserState(date, duration, it.next());
        }
    }

    private void runMonitorUserState(final Date date, final String duration, final AppUserState userState) {
        runMonitorCache(date, duration, userState.getSSH().getCache());
        runMonitorCache(date, duration, userState.getCIFS().getCache());
        runMonitorCache(date, duration, userState.getInterop().getCache());
        runMonitorCache(date, duration, userState.getWebDAV().getCache());
        runMonitorCache(date, duration, userState.getJDBC().getCache());
    }

    private void runMonitorCache(final Date date, final String duration, final ConnectionCache cache) {
        final Collection<ConnectionResource> resources = cache.getResources();
        for (final ConnectionResource resource : resources) {
            final Date dateIdle = DurationU.add(resource.getDateLast(), DateU.Const.TZ_GMT, duration);
            if (date.before(dateIdle)) {
                resource.getClass();
            } else {
                close(cache, resource);
            }
        }
    }

    private void close(final ConnectionCache cache, final ConnectionResource resource) {
        try {
            logger.info(resource.getName());
            cache.removeResource(resource.getName());
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }
    }

    private static class Const {
        private static final String DURATION = "PT15M";
    }
}
