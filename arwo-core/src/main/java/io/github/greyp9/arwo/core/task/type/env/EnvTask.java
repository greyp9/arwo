package io.github.greyp9.arwo.core.task.type.env;

import io.github.greyp9.arwo.core.task.core.Task;

import java.io.File;
import java.util.Date;
import java.util.Map;

public class EnvTask extends Task {
    private final String key;
    private final String value;
    private final Map<String, String> env;

    public EnvTask(final String name, final String key, final String value, final Map<String, String> env) {
        super(name, new Date());
        this.key = key;
        this.value = value;
        this.env = env;
    }

    public final String getKey() {
        return key;
    }

    public final String getValue() {
        return value;
    }

    public final Map<String, String> getEnv() {
        return env;
    }

    @Override
    public final Runnable createRunnable(final File ignored) {
        return new EnvRunnable(this);
    }
}
