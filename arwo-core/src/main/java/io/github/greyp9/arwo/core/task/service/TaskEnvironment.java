package io.github.greyp9.arwo.core.task.service;

import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.value.Value;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public final class TaskEnvironment {
    private final Map<String, String> env;

    public Map<String, String> getEnv() {
        return env;
    }

    public TaskEnvironment(final String env, final TaskService taskService) {
        this.env = new LinkedHashMap<>();
        final List<String> keys = Value.split(Http.Token.COMMA, env);
        for (String key : keys) {
            this.env.putAll(taskService.getEnv(key));
        }
    }
}
