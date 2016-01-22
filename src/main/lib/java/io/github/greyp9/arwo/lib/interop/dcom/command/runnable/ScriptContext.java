package io.github.greyp9.arwo.lib.interop.dcom.command.runnable;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.result.view.ResultsContext;
import io.github.greyp9.arwo.lib.interop.dcom.connection.InteropConnection;

import java.io.File;
import java.util.concurrent.ExecutorService;

public class ScriptContext {
    private final ExecutorService executorStream;
    private final ResultsContext resultsContext;
    private final InteropConnection connection;
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

    public final InteropConnection getConnection() {
        return connection;
    }

    public final long getPollInterval() {
        return pollInterval;
    }

    public ScriptContext(final ExecutorService executorStream, final ResultsContext resultsContext,
                         final InteropConnection connection, final long pollInterval) {
        this.executorStream = executorStream;
        this.resultsContext = resultsContext;
        this.connection = connection;
        this.pollInterval = pollInterval;
    }
}
