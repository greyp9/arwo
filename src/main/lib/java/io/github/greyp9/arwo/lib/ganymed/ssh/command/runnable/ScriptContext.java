package io.github.greyp9.arwo.lib.ganymed.ssh.command.runnable;

import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;

import java.util.concurrent.ExecutorService;

public class ScriptContext {
    private final SSHConnection sshConnection;
    private final ExecutorService executorStream;
    private final String requestPTY;
    private final long pollInterval;

    public final SSHConnection getConnection() {
        return sshConnection;
    }

    public final ExecutorService getExecutorStream() {
        return executorStream;
    }

    public final String getRequestPTY() {
        return requestPTY;
    }

    public final long getPollInterval() {
        return pollInterval;
    }

    public ScriptContext(final SSHConnection sshConnection, final ExecutorService executorStream) {
        this(sshConnection, executorStream, null, Const.POLL_INTERVAL);
    }

    public ScriptContext(final SSHConnection sshConnection, final ExecutorService executorStream,
                         final String requestPTY, final long pollInterval) {
        this.sshConnection = sshConnection;
        this.executorStream = executorStream;
        this.requestPTY = requestPTY;
        this.pollInterval = pollInterval;
    }

    private static class Const {
        private static final long POLL_INTERVAL = 10L;
    }
}
