package io.github.greyp9.arwo.core.vm.exec;

import io.github.greyp9.arwo.core.command.local.ScriptRunnable;
import io.github.greyp9.arwo.core.date.Interval;

import java.io.File;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.logging.Logger;

@SuppressWarnings("PMD.DoNotUseThreads")
public class UserExecutor {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final Principal principal;
    private final File folder;
    private final ExecutorService executorCommand;
    private final ExecutorService executorStream;
    private final Collection<Runnable> runnables;
    private final Map<String, Future<?>> futures;
    private final Interval interval;

    public UserExecutor(final Principal principal, final Date date, final File folder) {
        this.principal = principal;
        this.folder = folder;
        this.executorCommand = ExecutorServiceFactory.create(
                Const.N_THREAD_COMMAND, String.format("USER-%s", principal.getName()));
        this.executorStream = ExecutorServiceFactory.create(
                Const.N_THREAD_STREAMS, String.format("STREAM-%s", principal.getName()));
        this.runnables = new ArrayList<Runnable>();
        this.futures = new TreeMap<String, Future<?>>();
        this.interval = new Interval(date, null);
        logger.fine(String.format("[+][%s][%s]", executorCommand, executorStream));
    }

    public final Principal getPrincipal() {
        return principal;
    }

    public final File getFolder() {
        return folder;
    }

    public final ExecutorService getExecutorCommand() {
        return executorCommand;
    }

    public final Map<String, Future<?>> getFutures() {
        return futures;
    }

    public final ExecutorService getExecutorStream() {
        return executorStream;
    }

    public final Collection<Runnable> getRunnables() {
        return runnables;
    }

    public final boolean cancel(final String scriptID) {
        boolean cancelled = false;
        final Future<?> future = futures.get(scriptID);
        if (future != null) {
            cancelled = future.cancel(false);
            if (cancelled) {
                final Optional<ScriptRunnable> runnable = runnables.stream()
                        .filter(r -> r instanceof ScriptRunnable)
                        .map(ScriptRunnable.class::cast)
                        .filter(r -> r.getScript().getID().equals(scriptID))
                        .findFirst();
                runnable.ifPresent(r -> r.getScript().cancel());
            }
        }
        return cancelled;
    }

    public final Interval getInterval() {
        return interval;
    }

    public final void stop(final Date date) {
        interval.setDateFinish(date);
        executorCommand.shutdown();
        executorStream.shutdown();
        logger.fine(String.format("[-][%s][%s]", executorCommand, executorStream));
    }

    private static class Const {
        private static final int N_THREAD_COMMAND = 1;
        private static final int N_THREAD_STREAMS = 3;
    }
}
