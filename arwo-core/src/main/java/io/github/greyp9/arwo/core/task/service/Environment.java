package io.github.greyp9.arwo.core.task.service;

import java.util.LinkedHashMap;
import java.util.Map;

public class Environment {
    private final Map<String, String> env;

    /**
     * @implSpec Subclasses may override to allow deferred resolution of key/value pairs.
     */
    public Map<String, String> getEnv() {
        return env;
    }

    public Environment(final Map<String, String> env) {
        this.env = new LinkedHashMap<>(env);
    }
}
