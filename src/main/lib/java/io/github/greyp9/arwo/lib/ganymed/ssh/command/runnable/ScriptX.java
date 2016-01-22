package io.github.greyp9.arwo.lib.ganymed.ssh.command.runnable;

import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.result.view.ResultsContext;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;

import java.io.IOException;
import java.util.Date;
import java.util.concurrent.ExecutorService;

public class ScriptX {
    private final SSHConnection sshConnection;
    private final ExecutorService executorStream;
    private final ResultsContext resultsContext;

    public ScriptX(final SSHConnection sshConnection,
                   final ExecutorService executorStream, final ResultsContext resultsContext) {
        this.sshConnection = sshConnection;
        this.executorStream = executorStream;
        this.resultsContext = resultsContext;
    }

    public final Command runCommand(final String stdin) throws IOException {
        final ScriptContext context = new ScriptContext(executorStream, resultsContext, sshConnection);
        final Script script = new Script(null, new Date(), stdin);
        final ScriptRunnable runnable = new ScriptRunnable(script, context);
        runnable.run();  // synchronous
        return script.getCommands().iterator().next();
    }
}
