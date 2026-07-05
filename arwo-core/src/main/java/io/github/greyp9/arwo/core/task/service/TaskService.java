package io.github.greyp9.arwo.core.task.service;

import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.task.config.TaskServiceConfig;
import io.github.greyp9.arwo.core.task.core.Task;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.exec.ExecutorServiceFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public final class TaskService {
    private final String name;

    private final ExecutorService executorService;
    private final List<Task> tasks;
    private final List<Runnable> runnables;
    private final List<Future<?>> futures;
    private final Map<String, Map<String, String>> environments;
    private final String resource;
    private final File folderPersist;

    public String getName() {
        return name;
    }

    public List<Task> getTasks() {
        return tasks;
    }

    public List<Future<?>> getFutures() {
        return futures;
    }

    public List<Runnable> getRunnables() {
        return runnables;
    }

    public Map<String, String> getEnv(final String key) {
        return Value.defaultOnNull(environments.get(key), new HashMap<>());
    }

    public String getResource() {
        return resource;
    }

    public TaskService(final TaskServiceConfig config) {
        this.name = config.getName();
        this.executorService = ExecutorServiceFactory.create(config.getThreads(), getClass().getSimpleName());
        this.resource = config.getResource();
        this.tasks = new ArrayList<>();
        this.runnables = new ArrayList<>();
        this.futures = new ArrayList<>();
        this.environments = new HashMap<>();
        this.folderPersist = Value.isEmpty(config.getPersist()) ? null
                : FileU.ensureFolder(new File(SystemU.resolveSystemProperties(config.getPersist())));
        config.getEnvironments().forEach(
                (k, v) -> environments.put(k, v.getEnvironment()));
        Logger.getLogger(getClass().getName()).info("READY");
    }

    public Task submit(final Task task) {
        tasks.add(task);
        final Runnable runnable = task.createRunnable(folderPersist);
        runnables.add(runnable);
        final Future<?> future = executorService.submit(runnable);
        futures.add(future);
        task.setFuture(future);
        return task;
    }

    // ref `io.github.greyp9.arwo.core.exec.AppExecutorService`
    public List<Future<?>> getFuturesIncomplete() {
        return futures.stream().filter(f -> !f.isDone()).collect(Collectors.toList());
    }

    // ref `io.github.greyp9.arwo.core.exec.AppExecutorService`
    public boolean shutdownNow(final int secondsWait) {
        final Logger logger = Logger.getLogger(getClass().getName());
        logger.info("STOP");
        final List<Runnable> runnablesWait = this.executorService.shutdownNow();
        logger.info(String.format("STOP, wait for %d runnables", runnablesWait.size()));
        boolean awaitTermination = false;
        try {
            awaitTermination = executorService.awaitTermination(secondsWait, TimeUnit.SECONDS);
            logger.info(String.format("STOP, awaitTermination = %s", awaitTermination));
        } catch (InterruptedException e) {
            logger.severe(e.getMessage());
        }
        return awaitTermination;
    }
}
