package io.github.greyp9.arwo.core.task.service;

import io.github.greyp9.arwo.core.task.config.TaskServiceConfig;
import io.github.greyp9.arwo.core.task.core.Task;
import io.github.greyp9.arwo.core.vm.exec.ExecutorServiceFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.logging.Logger;

public final class TaskService {
    private final String name;

    private final ExecutorService executorService;
    private final List<Task> tasks;
    private final List<Runnable> runnables;
    private final List<Future<?>> futures;
    private final Map<String, Map<String, String>> environments;

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
        return environments.get(key);
    }

    public TaskService(final TaskServiceConfig config) {
        this.name = config.getName();
        this.executorService = ExecutorServiceFactory.create(config.getThreads(), getClass().getSimpleName());
        this.tasks = new ArrayList<>();
        this.runnables = new ArrayList<>();
        this.futures = new ArrayList<>();
        this.environments = new HashMap<>();
        config.getEnvironments().forEach(
                (k, v) -> environments.put(k, v.getEnvironment()));
        Logger.getLogger(getClass().getName()).info("READY");
    }

    public Task submit(final Task task) {
        tasks.add(task);
        final Runnable runnable = task.createRunnable();
        runnables.add(runnable);
        final Future<?> future = executorService.submit(runnable);
        futures.add(future);
        task.setFuture(future);
        return task;
    }
}
