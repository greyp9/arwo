package io.github.greyp9.arwo.core.runner;

import io.github.greyp9.arwo.core.vm.exec.ExecutorServiceFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.ExecutorService;

public final class TaskRunner {
    private final ExecutorService executor;
    private final long pollInterval;
    private final Collection<Runnable> tasks;

    public TaskRunner(final String name, final int nThreads, final long pollInterval) {
        this.executor = ExecutorServiceFactory.create(nThreads, name);
        this.pollInterval = pollInterval;
        this.tasks = Collections.synchronizedList(new ArrayList<>());
    }

    public long getPollInterval() {
        return pollInterval;
    }

    public CommandTask submit(final Command command) {
        final CommandTask commandTask = new CommandTask(executor, command, pollInterval);
        tasks.add(commandTask);
        executor.submit(commandTask);
        return commandTask;
    }
}
