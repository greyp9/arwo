package io.github.greyp9.arwo.core.io.command;

import java.util.Date;

public class CommandDone extends Command {
    private final String dir;
    private final String stdin;
    private final String stdout;
    private final String stderr;
    private final long dateStart;
    private final long dateFinish;
    private final Integer pid;
    private final Integer exitValue;

    @Override
    public final String getDir() {
        return dir;
    }

    @Override
    public final String getStdin() {
        return stdin;
    }

    @Override
    public final String getStdout() {
        return stdout;
    }

    @Override
    public final String getStderr() {
        return stderr;
    }

    @Override
    public final Date getStart() {
        return new Date(dateStart);
    }

    @Override
    public final Date getFinish() {
        return new Date(dateFinish);
    }

    @Override
    public final Integer getPID() {
        return pid;
    }

    @Override
    public final Integer getExitValue() {
        return exitValue;
    }

    public CommandDone(final CommandWork command, final String stdout, final String stderr,
                       final Date dateFinish, final Integer exitValue) {
        super();
        this.dir = command.getDir();
        this.stdin = command.getStdin();
        this.stdout = stdout;
        this.stderr = stderr;
        this.dateStart = command.getStart().getTime();
        this.dateFinish = dateFinish.getTime();
        this.pid = command.getPID();
        this.exitValue = exitValue;
    }
}
