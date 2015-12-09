package io.github.greyp9.arwo.lib.ganymed.ssh.command.runnable;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;

import java.io.File;
import java.util.concurrent.ExecutorService;

public class ScriptContext {
    private final SSHConnection sshConnection;
    private final ExecutorService executorStream;
    private final File folder;
    private final Locus locus;
    private final Alerts alerts;
    private final String requestPTY;
    private final long pollInterval;

    public final SSHConnection getSSHConnection() {
        return sshConnection;
    }

    public final ExecutorService getExecutorStream() {
        return executorStream;
    }

    public final File getFolder() {
        return folder;
    }

    public final Locus getLocus() {
        return locus;
    }

    public final Alerts getAlerts() {
        return alerts;
    }

    public final String getRequestPTY() {
        return requestPTY;
    }

    public final long getPollInterval() {
        return pollInterval;
    }

    public ScriptContext(final SSHConnection sshConnection, final ExecutorService executorStream) {
        this(sshConnection, executorStream, null, Const.POLL_INTERVAL, null, null, null);
    }

    public ScriptContext(final SSHConnection sshConnection, final ExecutorService executorStream,
                         final String requestPTY, final long pollInterval,
                         final File folder, final Locus locus, final Alerts alerts) {
        this.sshConnection = sshConnection;
        this.executorStream = executorStream;
        this.requestPTY = requestPTY;
        this.pollInterval = pollInterval;
        this.folder = folder;
        this.locus = locus;
        this.alerts = alerts;
    }

    private static class Const {
        private static final long POLL_INTERVAL = 10L;
    }
}
