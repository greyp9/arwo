package io.github.greyp9.arwo.lib.ganymed.ssh.connection;

import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.lib.ganymed.ssh.command.runnable.ScriptX;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ExecutorService;

public class SSHConnectionX {
    private final SSHConnection sshConnection;
    private final ExecutorService executorStream;

    public SSHConnectionX(final SSHConnection sshConnection, final ExecutorService executorStream) {
        this.sshConnection = sshConnection;
        this.executorStream = executorStream;
    }

    public final String toNameUID(final Integer uid) {
        return (uid == null) ? null : lookupCache(sshConnection.getUidToName(), uid, Const.UID_TO_NAME);
    }

    public final String toNameGID(final Integer gid) {
        return (gid == null) ? null : lookupCache(sshConnection.getGidToName(), gid, Const.GID_TO_NAME);
    }

    @SuppressWarnings("SynchronizationOnLocalVariableOrMethodParameter")
    private String lookupCache(final Map<Integer, String> map, final Integer id, final String commandPattern) {
        synchronized (map) {
            final String name = map.get(id);
            return (name == null) ? queryStore(map, id, commandPattern) : name;
        }
    }

    private String queryStore(final Map<Integer, String> map, final Integer id, final String commandPattern) {
        String value;
        try {
            final String stdin = String.format(commandPattern, id);
            final Command command = new ScriptX(sshConnection, executorStream).runCommand(stdin);
            final String[] resultsTokens = command.getStdout().split(":");
            value = ((resultsTokens.length > 0) ? resultsTokens[0] : id.toString());
        } catch (IOException e) {
            value = id.toString();
        }
        map.put(id, value);
        return value;
    }

    private static class Const {
        private static final String UID_TO_NAME = "getent passwd %d";
        private static final String GID_TO_NAME = "getent group %d";
    }
}
