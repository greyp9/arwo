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
import java.util.Optional;
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
            date = getNextTime(date, Const.DURATION_LOOP);
            logger.finest(String.format("pause:[%s]", XsdDateU.toXSDZMillis(date)));
            MutexU.waitUntil(reference, date);
            logger.finest(String.format("resume:[%s]", XsdDateU.toXSDZMillis(date)));
            if (reference.get() == null) {
                runMonitor(date);
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
        logger.finest(String.format("dateNext:BEGIN:[%s][%s]",
                userState.getPrincipal().getName(), XsdDateU.toXSDZMillis(dateNextIt)));
        final Collection<ConnectionCache> connectionCaches = userState.getConnectionCaches();
        for (final ConnectionCache connectionCache : connectionCaches) {
            for (final ConnectionResource resource : connectionCache.getResources()) {
                final String timeout = Optional.ofNullable(resource.getTimeout()).orElse(Const.DURATION_TIMEOUT);
                final Date dateIdle = DurationU.add(resource.getDateLast(), DateU.Const.TZ_GMT, timeout);
                dateNextIt = DateU.min(dateNext, dateIdle);
            }
        }
        logger.finest(String.format("dateNext:END:[%s][%s]",
                userState.getPrincipal().getName(), XsdDateU.toXSDZMillis(dateNextIt)));
        return dateNextIt;
    }

    private void runMonitor(final Date date) {
        final Iterator<AppUserState> it = appState.getIterator();
        while (it.hasNext()) {
            runMonitorUserState(date, it.next());
        }
    }

    private void runMonitorUserState(final Date date, final AppUserState userState) {
        final Collection<ConnectionCache> connectionCaches = userState.getConnectionCaches();
        // logger.finest(String.format("runMonitorUserState:BEGIN:[%s]", userState.getPrincipal().getName()));
        for (final ConnectionCache connectionCache : connectionCaches) {
            runMonitorCache(date, userState.getPrincipal().getName(), connectionCache);
        }
        // logger.finest(String.format("runMonitorUserState:END:[%s]", userState.getPrincipal().getName()));
    }

    private void runMonitorCache(final Date date, final String name, final ConnectionCache cache) {
        // logger.info(String.format("runMonitorCache:BEGIN:[%s][%s][%s]",
        //        name, cache.getName(), XsdDateU.toXSDZMillis(date)));
        final Collection<ConnectionResource> resources = cache.getResources();
        for (final ConnectionResource resource : resources) {
            final String timeout = Optional.ofNullable(resource.getTimeout()).orElse(Const.DURATION_TIMEOUT);
            final Date dateIdle = DurationU.add(resource.getDateLast(), DateU.Const.TZ_GMT, timeout);
            // logger.info(String.format("[%s][%s][%s]", resource.getName(),
            //        XsdDateU.toXSDZMillis(dateIdle), XsdDateU.toXSDZMillis(dateIdle)));
            if (date.compareTo(dateIdle) >= 0) {
                close(cache, resource, resource.getDateLast());
            }
        }
        // logger.info(String.format("runMonitorCache:END:[%s][%s][%s]",
        //        name, cache.getName(), XsdDateU.toXSDZMillis(date)));
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
        private static final String DURATION_LOOP = "PT5M";  // i18n
        private static final String DURATION_TIMEOUT = "PT15M";  // i18n
    }
}
