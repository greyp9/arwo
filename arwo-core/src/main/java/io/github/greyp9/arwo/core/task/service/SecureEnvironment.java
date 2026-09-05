package io.github.greyp9.arwo.core.task.service;

import io.github.greyp9.arwo.core.envsec.store.SecureStore;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SecureEnvironment extends Environment {
    private final SecureStore secureStore;

    @Override
    public final Map<String, String> getEnv() {
        final Map<String, String> env = new LinkedHashMap<>();
        for (Map.Entry<String, String> entry : super.getEnv().entrySet()) {
            final Matcher matcher = PATTERN_SECURE.matcher(entry.getValue());
            if (matcher.matches()) {
                env.put(entry.getKey(), secureStore.getPropertySafe(matcher.group(1)));
            } else {
                env.put(entry.getKey(), entry.getValue());
            }
        }
        return env;
    }

    public SecureEnvironment(final Map<String, String> env, final SecureStore secureStore) {
        super(env);
        this.secureStore = secureStore;
    }

    private static final Pattern PATTERN_SECURE = Pattern.compile("secure\\((.*)\\)");
}
