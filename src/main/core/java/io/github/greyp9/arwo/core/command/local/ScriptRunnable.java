package io.github.greyp9.arwo.core.command.local;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.io.command.CommandToDo;
import io.github.greyp9.arwo.core.io.command.CommandWork;
import io.github.greyp9.arwo.core.io.runnable.InputStreamRunnable;
import io.github.greyp9.arwo.core.io.runnable.OutputStreamRunnable;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.io.script.write.ScriptWriter;
import io.github.greyp9.arwo.core.lang.StringU;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;
import io.github.greyp9.arwo.core.vm.process.ProcessU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.concurrent.ExecutorService;
import java.util.logging.Logger;

@SuppressWarnings("PMD.DoNotUseThreads")
public class ScriptRunnable implements Runnable {
    private final Script script;
    private final ScriptContext context;

    public ScriptRunnable(final Script script, final ScriptContext context) {
        this.script = script;
        this.context = context;
    }

    @Override
    public final void run() {
        try {
            script.start();
            runInner();
        } catch (IOException e) {
            Logger.getLogger(getClass().getName()).severe(e.getMessage());
        } finally {
            script.finish();
        }
        try {
            new ScriptWriter(script, context.getLocus()).writeTo(script.getFile(context.getFolder()));
        } catch (IOException e) {
            context.getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
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
        CommandWork commandWork = script.startCommand(commandToDo, UTF8Codec.Const.UTF8);
        Integer exitValue = null;
        try {
            final String[] commandArray = StringU.tokenize(commandWork.getStdin(), StringU.Const.WHITESPACE);
            final Runtime runtime = Runtime.getRuntime();
            final Process process = runtime.exec(commandArray, null, context.getUserDir());
            final Integer processId = ProcessU.getProcessId(process);
            commandWork = script.updateCommand(commandWork, processId);
            exitValue = monitorCommand(commandWork, process);
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
}
