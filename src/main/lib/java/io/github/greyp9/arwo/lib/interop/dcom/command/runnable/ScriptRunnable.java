package io.github.greyp9.arwo.lib.interop.dcom.command.runnable;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.io.command.CommandToDo;
import io.github.greyp9.arwo.core.io.command.CommandWork;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.io.script.write.ScriptWriter;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;
import io.github.greyp9.arwo.lib.interop.dcom.command.core.InputTextStream;
import io.github.greyp9.arwo.lib.interop.dcom.command.core.OutputTextStream;
import io.github.greyp9.arwo.lib.interop.dcom.command.core.WshScriptExec;
import io.github.greyp9.arwo.lib.interop.dcom.command.core.WshShell;
import io.github.greyp9.arwo.lib.interop.dcom.connection.InteropConnection;
import org.jinterop.dcom.common.JIException;
import org.jinterop.dcom.core.JISession;

import java.io.IOException;
import java.util.concurrent.Future;
import java.util.logging.Logger;

@SuppressWarnings("PMD.DoNotUseThreads")
public class ScriptRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());
    private final ScriptContext context;
    private final Script script;
    private final Locus locus;
    private final Alerts alerts;

    public ScriptRunnable(final ScriptContext context, final Script script) {
        this.context = context;
        this.script = script;
        this.locus = context.getLocus();
        this.alerts = context.getAlerts();
    }

    @Override
    public final void run() {
        try {
            logger.entering(getClass().getName(), Runnable.class.getName());
            script.start();
            runInner();
        } catch (JIException e) {
            alerts.add(new Alert(Alert.Severity.ERR, e.getMessage(), e.getClass().getSimpleName(), null));
        } catch (IOException e) {
            alerts.add(new Alert(Alert.Severity.ERR, e.getMessage(), e.getClass().getSimpleName(), null));
        } finally {
            script.finish();
        }
        try {
            new ScriptWriter(script, locus).writeTo(context.getFile());
        } catch (IOException e) {
            alerts.add(new Alert(Alert.Severity.ERR, e.getMessage()));
        } finally {
            logger.exiting(getClass().getName(), Runnable.class.getName());
        }
    }

    private void runInner() throws IOException, JIException {
        final InteropConnection connection = context.getConnection();
        final JISession session = WshShell.Factory.createSession("", connection.getUser(), connection.getPassword());
        try {
            runInner2(session, connection.getHost());
        } finally {
            WshShell.Factory.destroySession(session);
        }
    }

    private void runInner2(final JISession session, final String host) throws JIException, IOException {
        final WshShell shell = WshShell.Factory.createShell(host, session);
        try {
            runInner3(shell);
        } finally {
            WshShell.Factory.releaseShell(shell);
        }
    }

    @SuppressWarnings("PMD.AssignmentInOperand")
    private void runInner3(final WshShell shell) throws JIException, IOException {
        final String dir = shell.getCurrentDirectory();
        CommandToDo commandToDo;
        while ((commandToDo = script.getCommandToDo()) != null) {
            runCommand(shell, commandToDo, dir);
        }
        // notify caller thread
        MutexU.notifyAll(this);
    }

    private void runCommand(
            final WshShell shell, final CommandToDo commandToDo, final String dir) throws IOException, JIException {
        // start process
        CommandWork commandWork = script.startCommand(commandToDo, UTF8Codec.Const.UTF8, dir);
        final WshScriptExec exec = shell.exec(commandWork.getStdin());
        if (exec != null) {
            commandWork = script.updateCommand(commandWork, exec.getPID());
            monitorCommand(exec, commandWork);
            exec.getDispatch().release();
        }
        // notify caller thread
        MutexU.notifyAll(this);
    }

    private void monitorCommand(final WshScriptExec exec, final CommandWork commandWork)
            throws IOException, JIException {
        // attach to process streams
        final long interval = context.getPollInterval();
        final ByteBuffer bufferStdin = commandWork.getByteBufferStdin();
        final ByteBuffer bufferStdout = commandWork.getByteBufferStdout();
        final ByteBuffer bufferStderr = commandWork.getByteBufferStderr();
        final OutputTextStream stdin = exec.getStdin();
        final InputTextStream stdout = exec.getStdout();
        final InputTextStream stderr = exec.getStderr();
        final OutputStreamRunnable runnableStdin = new OutputStreamRunnable(stdin, bufferStdin, interval);
        final InputStreamRunnable runnableStdout = new InputStreamRunnable(stdout, bufferStdout, interval);
        final InputStreamRunnable runnableStderr = new InputStreamRunnable(stderr, bufferStderr, interval);
        final Future<?> futureStdin = context.getExecutorStream().submit(runnableStdin);
        final Future<?> futureStdout = context.getExecutorStream().submit(runnableStdout);
        // monitor process until it ends
        int status = 0;
        while (status == 0) {
            ThreadU.sleepMillis(interval);
            if (script.isInterrupted()) {
                exec.terminate();
            }
            status = exec.getStatus();
        }
        // collect process data
        final int exitCode = exec.getExitCode();
        // signal stream threads
        runnableStdin.setFinished();
        while (!futureStdin.isDone()) {
            ThreadU.sleepMillis(interval);
        }
        // signal stream threads
        runnableStdout.setFinished();
        while (!futureStdout.isDone()) {
            ThreadU.sleepMillis(interval);
        }
        // stderr data must be collected after stdout (can't run both at same time; blocking)
        final Future<?> futureStderr = context.getExecutorStream().submit(runnableStderr);
        runnableStderr.setFinished();
        while (!futureStderr.isDone()) {
            ThreadU.sleepMillis(interval);
        }
        // mark process
        script.finishCommand(commandWork, exitCode);
        context.getConnection().update(commandWork.getStart());
    }
}
