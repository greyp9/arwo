package io.github.greyp9.arwo.core.task.config;

import java.util.Map;

public final class EnvironmentConfig {
    private final String name;
    private final Map<String, String> environment;

    public String getName() {
        return name;
    }

    public Map<String, String> getEnvironment() {
        return environment;
    }

    public EnvironmentConfig(final String name, final Map<String, String> environment) {
        this.name = name;
        this.environment = environment;
    }
}
