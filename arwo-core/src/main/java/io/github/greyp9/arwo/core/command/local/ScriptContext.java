package io.github.greyp9.arwo.core.command.local;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.result.view.ResultsContext;

import java.io.File;
import java.util.concurrent.ExecutorService;

public class ScriptContext {
    private final ExecutorService executorStream;
    private final ResultsContext resultsContext;
    private final File userDir;
    private final long pollInterval;

    public final ExecutorService getExecutorStream() {
        return executorStream;
    }

    public final Locus getLocus() {
        return resultsContext.getLocus();
    }

    public final Bundle getBundle() {
        return resultsContext.getBundle();
    }

    public final Alerts getAlerts() {
        return resultsContext.getAlerts();
    }

    public final File getFile() {
        return resultsContext.getMetaLink().getFile();
    }

    public final String getHref() {
        return resultsContext.getMetaLink().getHref();
    }

    public final File getUserDir() {
        return userDir;
    }

    public final long getPollInterval() {
        return pollInterval;
    }

    public ScriptContext(
            final ExecutorService executorStream, final ResultsContext resultsContext,
            final File userDir) {
        this(executorStream, resultsContext, Const.POLL_INTERVAL, userDir);
    }

    public ScriptContext(
            final ExecutorService executorStream, final ResultsContext resultsContext,
            final long pollInterval, final File userDir) {
        this.executorStream = executorStream;
        this.resultsContext = resultsContext;
        this.pollInterval = pollInterval;
        this.userDir = userDir;
    }

    private static class Const {
        private static final long POLL_INTERVAL = 10L;
    }
}
