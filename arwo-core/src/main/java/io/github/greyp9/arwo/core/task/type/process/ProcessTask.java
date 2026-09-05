package io.github.greyp9.arwo.core.task.type.process;

import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.task.core.Task;

import java.io.File;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

public class ProcessTask extends Task {
    private final List<String> cmd;
    private final boolean shell;
    private final Map<String, String> env;
    private final File dir;
    private final Charset charset;
    private final ByteBuffer stdin;
    private final ByteBuffer stdout;
    private final ByteBuffer stderr;

    private Long pid;

    public final String[] getCmd() {
        return cmd.toArray(new String[0]);
    }

    public final boolean getShell() {
        return shell;
    }

    public final Map<String, String> getEnv() {
        return env;
    }

    public final void setEnv(final String key, final String value) {
        env.put(key, value);
    }

    public final File getDir() {
        return dir;
    }

    public final ByteBuffer getStdin() {
        return stdin;
    }

    public final ByteBuffer getStdout() {
        return stdout;
    }

    public final ByteBuffer getStderr() {
        return stderr;
    }

    public final Long getPid() {
        return pid;
    }

    public final void setPid(final Long pid) {
        this.pid = pid;
    }

    public final boolean isRunning() {
        return ((pid != null) && (getExitValue() == null));
    }

    public ProcessTask(final String name, final Date dateSubmit, final String cmd1,
                       final boolean shell, final Map<String, String> env, final File dir) {
        this(name, dateSubmit, Collections.singletonList(cmd1), shell, env, dir);
    }

    public ProcessTask(final String name, final Date dateSubmit, final List<String> cmd,
                       final boolean shell, final Map<String, String> env, final File dir) {
        super(name, dateSubmit);
        this.cmd = cmd;
        this.shell = shell;
        this.env = env;
        this.dir = dir;
        this.charset = StandardCharsets.UTF_8;
        this.stdin = new ByteBuffer(charset);
        this.stdout = new ByteBuffer(charset);
        this.stderr = new ByteBuffer(charset);
    }

    @SuppressWarnings("checkstyle:parameternumber")
    public ProcessTask(final String name, final Date dateSubmit, final List<String> cmd,
                       final boolean shell, final Map<String, String> env, final File dir,
                       final ByteBuffer stdout, final ByteBuffer stderr) {
        super(name, dateSubmit);
        this.cmd = cmd;
        this.shell = shell;
        this.env = env;
        this.dir = dir;
        this.charset = null;
        this.stdin = new ByteBuffer(StandardCharsets.UTF_8);
        this.stdout = stdout;
        this.stderr = stderr;
    }

    @Override
    public final Runnable createRunnable(final File folderPersist) {
        return new ProcessRunnable(this, folderPersist);
    }

    public static class Const {
        public static final String FIELD_COMMAND = "command";
        public static final String FIELD_EXIT_VALUE = "exitValue";
        public static final String FIELD_PID = "pid";

        public static final String STREAM_STDIN = "stdin";
        public static final String STREAM_STDERR = "stderr";
        public static final String STREAM_STDOUT = "stdout";
    }
}
