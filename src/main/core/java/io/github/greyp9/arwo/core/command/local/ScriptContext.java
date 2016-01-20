package io.github.greyp9.arwo.core.command.local;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.locus.Locus;

import java.io.File;
import java.util.concurrent.ExecutorService;

public class ScriptContext {
    private final ExecutorService executorStream;
    private final File userDir;
    private final File file;
    private final Locus locus;
    private final Alerts alerts;
    private final long pollInterval;

    public final ExecutorService getExecutorStream() {
        return executorStream;
    }

    public final File getUserDir() {
        return userDir;
    }

    public final File getFile() {
        return file;
    }

    public final Locus getLocus() {
        return locus;
    }

    public final Alerts getAlerts() {
        return alerts;
    }

    public final long getPollInterval() {
        return pollInterval;
    }

    public ScriptContext(final ExecutorService executorStream) {
        this(executorStream, Const.POLL_INTERVAL, null, null, null, null);
    }

    public ScriptContext(final ExecutorService executorStream, final long pollInterval, final File userDir,
                         final File file, final Locus locus, final Alerts alerts) {
        this.executorStream = executorStream;
        this.pollInterval = pollInterval;
        this.userDir = userDir;
        this.file = file;
        this.locus = locus;
        this.alerts = alerts;
    }

    private static class Const {
        private static final long POLL_INTERVAL = 10L;
    }
}
