package io.github.greyp9.arwo.core.task.config;

import java.util.HashMap;
import java.util.Map;

public class TaskServiceConfig {
    private final String name;
    private final int threads;
    private final String resource;
    private final String persist;
    private final Map<String, EnvironmentConfig> environments;

    public final String getName() {
        return name;
    }

    public final int getThreads() {
        return threads;
    }

    public final String getResource() {
        return resource;
    }

    public final String getPersist() {
        return persist;
    }

    public final Map<String, EnvironmentConfig> getEnvironments() {
        return environments;
    }

    public TaskServiceConfig(final String name, final int threads, final String resource, final String persist) {
        this.name = name;
        this.threads = threads;
        this.resource = resource;
        this.persist = persist;
        this.environments = new HashMap<>();
    }

    public final void addEnvironment(final EnvironmentConfig environment) {
        this.environments.put(environment.getName(), environment);
    }
}
