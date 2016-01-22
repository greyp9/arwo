package io.github.greyp9.arwo.lib.ganymed.ssh.command.runnable;

import ch.ethz.ssh2.ChannelCondition;
import ch.ethz.ssh2.Session;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.write.AlertWriter;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.io.command.CommandToDo;
import io.github.greyp9.arwo.core.io.command.CommandWork;
import io.github.greyp9.arwo.core.io.runnable.InputStreamRunnable;
import io.github.greyp9.arwo.core.io.runnable.OutputStreamRunnable;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.io.script.write.ScriptWriter;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.concurrent.ExecutorService;
import java.util.logging.Logger;

@SuppressWarnings("PMD.DoNotUseThreads")
public class ScriptRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());
    private final Script script;
    private final ScriptContext context;

    public ScriptRunnable(final Script script, final ScriptContext context) {
        this.script = script;
        this.context = context;
    }

    @Override
    public final void run() {
        final Locus locus = context.getLocus();
        final Bundle bundle = context.getBundle();
        final Alerts alerts = context.getAlerts();
        final File file = context.getFile();
        final String href = context.getHref();
        try {
            logger.entering(getClass().getName(), Runnable.class.getName());
            script.start();
            runInner();
        } catch (IOException e) {
            alerts.add(new Alert(Alert.Severity.ERR, e.getMessage(), e.getClass().getSimpleName()));
        } finally {
            script.finish();
        }
        try {
            new ScriptWriter(script, locus).writeTo(file);
            new AlertWriter(bundle, alerts).write("command.finished", "results.view", href);
        } catch (IOException e) {
            alerts.add(new Alert(Alert.Severity.ERR, e.getMessage()));
        } finally {
            logger.exiting(getClass().getName(), Runnable.class.getName());
        }
    }

    @SuppressWarnings("PMD.AssignmentInOperand")
    private void runInner() throws IOException {
        CommandToDo command;
        while ((command = script.getCommandToDo()) != null) {
            runCommand(command);
        }
        MutexU.notifyAll(this);
    }

    private void runCommand(final CommandToDo commandToDo) throws IOException {
        final SSHConnection sshConnection = context.getSSHConnection();
        final String requestPTY = context.getRequestPTY();
        final Session session = sshConnection.getConnection().openSession();
        try {
            if (requestPTY != null) {
                session.requestPTY(requestPTY, Const.TERMINAL_WIDTH, Const.TERMINAL_HEIGHT, 0, 0, null);
            }
            runCommand(commandToDo, session);
        } finally {
            session.close();
        }
    }

    private void runCommand(final CommandToDo commandToDo, final Session session) throws IOException {
        final CommandWork commandWork = script.startCommand(commandToDo, UTF8Codec.Const.UTF8, null);
        Integer exitValue = null;
        try {
            session.execCommand(commandWork.getStdin());
            exitValue = monitorCommand(commandWork, session);
        } catch (IOException e) {
            commandWork.getByteBufferStderr().addString(e.getMessage());
        } finally {
            script.finishCommand(commandWork, exitValue);
            context.getSSHConnection().update(commandWork.getStart());
        }
    }

    @SuppressWarnings({ "PMD.GuardLogStatementJavaUtil", "PMD.GuardLogStatement" })
    private Integer monitorCommand(final CommandWork commandWork, final Session session) throws IOException {
        final ExecutorService executorStream = context.getExecutorStream();
        final long pollInterval = context.getPollInterval();
        final ByteBuffer bufferStdin = commandWork.getByteBufferStdin();
        final ByteBuffer bufferStdout = commandWork.getByteBufferStdout();
        final ByteBuffer bufferStderr = commandWork.getByteBufferStderr();
        final OutputStream stdin = session.getStdin();
        final InputStream stdout = session.getStdout();
        final InputStream stderr = session.getStderr();
        // allow for supplemental process input
        final OutputStreamRunnable runnableStdin = new OutputStreamRunnable(stdin, bufferStdin, pollInterval);
        executorStream.execute(runnableStdin);
        // monitor process
        final InputStreamRunnable runnableStdout = new InputStreamRunnable(stdout, bufferStdout, pollInterval);
        final InputStreamRunnable runnableStderr = new InputStreamRunnable(stderr, bufferStderr, pollInterval);
        final InputStreamRunnable[] streams = { runnableStdout, runnableStderr };
        for (final InputStreamRunnable stream : streams) {
            executorStream.execute(stream);
        }
        // monitor process
        final int conditionExit = session.waitForCondition(ChannelCondition.EXIT_STATUS, 0L);
        logger.finest("conditionExitStatus/" + conditionExit);  // i18n
        final int conditionData = waitForData(session, runnableStdout.getPollInterval());
        logger.finest("conditionData/" + conditionData);  // i18n
        final Integer exitValue = session.getExitStatus();
        // allow process complete
        runnableStdin.stop();
        for (final InputStreamRunnable stream : streams) {
            stream.waitForComplete();
        }
        // notify caller thread
        MutexU.notifyAll(this);
        return exitValue;
    }

    private int waitForData(final Session session, final long pollInterval) throws IOException {
        int condition;
        do {
            ThreadU.sleepMillis(pollInterval);
            condition = session.waitForCondition(Const.DATA, pollInterval);
        } while ((condition & Const.DATA) != 0);
        return condition;
    }

    private static class Const {
        private static final int TERMINAL_HEIGHT = 24;
        private static final int TERMINAL_WIDTH = 80;
        private static final int DATA = ChannelCondition.STDOUT_DATA | ChannelCondition.STDERR_DATA;
    }
}
