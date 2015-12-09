package io.github.greyp9.arwo.core.command.local;

import java.io.File;
import java.util.concurrent.ExecutorService;

public class ScriptContext {
    private final ExecutorService executorStream;
    private final File userDir;
    private final long pollInterval;

    public final ExecutorService getExecutorStream() {
        return executorStream;
    }

    public final File getUserDir() {
        return userDir;
    }

    public final long getPollInterval() {
        return pollInterval;
    }

    public ScriptContext(final ExecutorService executorStream, final File userDir) {
        this.executorStream = executorStream;
        this.userDir = userDir;
        this.pollInterval = Const.POLL_INTERVAL;
    }

    private static class Const {
        private static final long POLL_INTERVAL = 10L;
    }
}
