package io.github.greyp9.arwo.core.task.service;

import io.github.greyp9.arwo.core.envsec.store.SecureStore;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.value.Value;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class TaskEnvironment {
    private final Map<String, String> env;

    public Map<String, String> getEnv() {
        return env;
    }

    public TaskEnvironment(final String env, final TaskService taskService, final SecureStore secureStore) {
        this.env = new HashMap<>();
        final List<String> keys = Value.split(Http.Token.COMMA, env);
        for (String key : keys) {
            put(this.env, taskService.getEnv(key), secureStore);
        }
        Logger.getLogger(getClass().getName()).info(String.format("toEnv():[%d]", this.env.size()));
    }

    private static void put(final Map<String, String> target,
                            final Map<String, String> source,
                            final SecureStore secureStore) {
        for (Map.Entry<String, String> entry : source.entrySet()) {
            final Matcher matcher = PATTERN_SECURE.matcher(entry.getValue());
            if (matcher.matches()) {
                target.put(entry.getKey(), secureStore.getPropertySafe(matcher.group(1)));
            } else {
                target.put(entry.getKey(), entry.getValue());
            }
        }
    }

    private static final Pattern PATTERN_SECURE = Pattern.compile("secure\\((.*)\\)");
}
