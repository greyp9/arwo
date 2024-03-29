package io.github.greyp9.arwo.app.daemon.connect.runnable;

import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.connect.ConnectionResource;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
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
            logger.finest("pause");  // i18n log
            MutexU.waitUntil(reference, date);
            logger.finest("resume");  // i18n log
            if (reference.get() == null) {
                runMonitor(date, Const.DURATION);  // at PT14M59S, age out PT15M stale connections
                date = new Date();
            }
        }
        logger.finer(reference::get);
        logger.exiting(getClass().getName(), Runnable.class.getName());
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
        runMonitorCache(date, duration, userState.getMail().getCacheSMTP());
        runMonitorCache(date, duration, userState.getMail().getCacheIMAP());
    }

    private void runMonitorCache(final Date date, final String duration, final ConnectionCache cache) {
        final Collection<ConnectionResource> resources = cache.getResources();
        for (final ConnectionResource resource : resources) {
            final Date dateIdle = DurationU.add(resource.getDateLast(), DateU.Const.TZ_GMT, duration);
            if (date.before(dateIdle)) {
                resource.getClass();
            } else {
                close(cache, resource, resource.getDateLast());
            }
        }
    }

    private void close(final ConnectionCache cache, final ConnectionResource resource, final Date dateLast) {
        try {
            logger.info(String.format("%s [%s]", resource.getName(), XsdDateU.toXSDZMillis(dateLast)));
            cache.removeResource(resource.getName());
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }
    }

    private static class Const {
        private static final String DURATION = "PT15M";  // i18n
    }
}
