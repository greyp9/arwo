package io.github.greyp9.arwo.core.vm.exec;

import io.github.greyp9.arwo.core.date.Interval;

import java.io.File;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.concurrent.ExecutorService;
import java.util.logging.Logger;

@SuppressWarnings("PMD.DoNotUseThreads")
public class UserExecutor {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final Principal principal;
    private final File folder;
    private final ExecutorService executorCommand;
    private final ExecutorService executorStream;
    private final Collection<Runnable> runnables;
    private final Interval interval;

    public UserExecutor(final Principal principal, final Date date, final File folder) {
        this.principal = principal;
        this.folder = folder;
        this.executorCommand = ExecutorServiceFactory.create(
                Const.N_THREAD_COMMAND, String.format("USER-%s", principal.getName()));
        this.executorStream = ExecutorServiceFactory.create(
                Const.N_THREAD_STREAMS, String.format("STREAM-%s", principal.getName()));
        this.runnables = new ArrayList<Runnable>();
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

    public final ExecutorService getExecutorStream() {
        return executorStream;
    }

    public final Collection<Runnable> getRunnables() {
        return runnables;
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
