package io.github.greyp9.arwo.lib.ganymed.ssh.command.runnable;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.result.view.ResultsContext;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;

import java.io.File;
import java.util.concurrent.ExecutorService;

public class ScriptContext {
    private final ExecutorService executorStream;
    private final ResultsContext resultsContext;
    private final SSHConnection sshConnection;
    private final String requestPTY;
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

    public final SSHConnection getSSHConnection() {
        return sshConnection;
    }

    public final String getRequestPTY() {
        return requestPTY;
    }

    public final long getPollInterval() {
        return pollInterval;
    }

    public ScriptContext(final ExecutorService executorStream, final ResultsContext resultsContext,
                         final SSHConnection sshConnection) {
        this(executorStream, resultsContext, sshConnection, null, Const.POLL_INTERVAL);
    }

    public ScriptContext(final ExecutorService executorStream, final ResultsContext resultsContext,
                         final SSHConnection sshConnection, final String requestPTY, final long pollInterval) {
        this.sshConnection = sshConnection;
        this.executorStream = executorStream;
        this.requestPTY = requestPTY;
        this.pollInterval = pollInterval;
        this.resultsContext = resultsContext;
    }

    private static class Const {
        private static final long POLL_INTERVAL = 10L;
    }
}
