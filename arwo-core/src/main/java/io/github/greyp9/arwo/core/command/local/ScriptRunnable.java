package io.github.greyp9.arwo.core.command.local;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.write.AlertWriter;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.io.command.CommandToDo;
import io.github.greyp9.arwo.core.io.command.CommandWork;
import io.github.greyp9.arwo.core.io.runnable.InputStreamRunnable;
import io.github.greyp9.arwo.core.io.runnable.OutputStreamRunnable;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.io.script.write.ScriptWriter;
import io.github.greyp9.arwo.core.lang.ShellU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.exec.ThreadPoolU;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;
import io.github.greyp9.arwo.core.vm.process.ProcessU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.ExecutorService;
import java.util.logging.Level;
import java.util.logging.Logger;

@SuppressWarnings("PMD.DoNotUseThreads")
public class ScriptRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());
    private final Script script;
    private final ScriptContext context;
    private Collection<CommandWork> commands;

    public final void setCommands(final Collection<CommandWork> commands) {
        this.commands = commands;
    }

    public final Script getScript() {
        return script;
    }

    public ScriptRunnable(final Script script, final ScriptContext context) {
        this.script = script;
        this.context = context;
        this.commands = new ArrayList<>();
    }

    @Override
    public final void run() {
        final Locus locus = context.getLocus();
        final Bundle bundle = context.getBundle();
        final Alerts alerts = context.getAlerts();
        final File file = context.getFile();
        final String href = context.getHref();
        try {
            checkThreadPool(context.getExecutorStream());
            logger.entering(getClass().getName(), Runnable.class.getName());
            script.start();
            runInner();
        } catch (final Throwable e) {
            logger.throwing(getClass().getName(), null, e);
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

    private void checkThreadPool(final ExecutorService executorService) throws IOException {
        // need 3 available threads for streams (stdin, stdout, stderr)
        final boolean isAvailable = ThreadPoolU.isAvailablePool(executorService, Const.N_THREAD_STREAMS);
        final Level level = isAvailable ? Level.FINE : Level.WARNING;
        logger.log(level, ThreadPoolU.getTelemetry(executorService));
        if (!isAvailable) {
            throw new IOException(new IllegalStateException("insufficient threads available"));
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
        final File dir = Value.defaultOnNull(context.getUserDir(), new File(SystemU.userDir()));
        CommandWork commandWork = script.startCommand(commandToDo, StandardCharsets.UTF_8, FileU.fromFile(dir));
        Integer exitValue = null;
        try {
            final String[] commandArray = ShellU.toCommandArray(commandWork.getStdin());
            logger.fine(String.format("COMMAND:[%s], USERDIR=[%s]",
                    Arrays.asList(commandArray).toString(), dir.getAbsolutePath()));
            final Runtime runtime = Runtime.getRuntime();
            final Process process = runtime.exec(commandArray, null, dir);
            final Long processId = ProcessU.getProcessId(process);
            commandWork = script.updateCommand(commandWork, processId);

            // IN PROGRESS COMMANDS ARE AVAILABLE TO CHECK PROGRESS
            try {
                commands.add(commandWork);
                exitValue = monitorCommand(commandWork, process);
            } finally {
                commands.remove(commandWork);
            }

        } catch (IOException e) {
            commandWork.getByteBufferStderr().addString(e.getMessage());
        } finally {
            script.finishCommand(commandWork, exitValue);
        }
    }

    private Integer monitorCommand(final CommandWork commandWork, final Process process) throws IOException {
        final ExecutorService executorStream = context.getExecutorStream();
        final long pollInterval = context.getPollInterval();
        final ByteBuffer bufferStdin = commandWork.getByteBufferStdin();
        final ByteBuffer bufferStdout = commandWork.getByteBufferStdout();
        final ByteBuffer bufferStderr = commandWork.getByteBufferStderr();
        final OutputStream stdin = process.getOutputStream();
        final InputStream stdout = process.getInputStream();
        final InputStream stderr = process.getErrorStream();
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
        Integer exitValue = null;
        while (exitValue == null) {
            ThreadU.sleepMillis(pollInterval);
            exitValue = isProcessFinished(process);
        }
        // allow process complete
        runnableStdin.stop();
        for (final InputStreamRunnable stream : streams) {
            stream.waitForComplete();
        }
        // notify caller thread
        MutexU.notifyAll(this);
        return exitValue;
    }

    @SuppressWarnings("PMD.OnlyOneReturn")
    private static Integer isProcessFinished(final Process process) {
        try {
            return process.exitValue();
        } catch (IllegalThreadStateException e) {
            return null;
        }
    }

    private static class Const {
        private static final int N_THREAD_STREAMS = 3;
    }
}
