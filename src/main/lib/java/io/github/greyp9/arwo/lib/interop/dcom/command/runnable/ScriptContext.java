package io.github.greyp9.arwo.lib.interop.dcom.command.runnable;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.lib.interop.dcom.connection.InteropConnection;

import java.io.File;
import java.util.concurrent.ExecutorService;

public class ScriptContext {
    private final InteropConnection connection;
    private final ExecutorService executorStream;
    private final File file;
    private final Locus locus;
    private final Alerts alerts;

    private final long pollInterval;

    public final InteropConnection getConnection() {
        return connection;
    }

    public final ExecutorService getExecutorStream() {
        return executorStream;
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

    public ScriptContext(final InteropConnection connection, final ExecutorService executorStream,
                         final File file, final Locus locus, final Alerts alerts, final long pollInterval) {
        this.connection = connection;
        this.executorStream = executorStream;
        this.file = file;
        this.locus = locus;
        this.alerts = alerts;
        this.pollInterval = pollInterval;
    }
}
