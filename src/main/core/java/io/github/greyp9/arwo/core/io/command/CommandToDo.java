package io.github.greyp9.arwo.core.io.command;

import java.util.Date;

public class CommandToDo extends Command {
    private final String stdin;

    @Override
    public final String getDir() {
        return null;
    }

    @Override
    public final String getStdin() {
        return stdin;
    }

    @Override
    public final String getStdout() {
        return null;
    }

    @Override
    public final String getStderr() {
        return null;
    }

    @Override
    public final Date getStart() {
        return null;
    }

    @Override
    public final Date getFinish() {
        return null;
    }

    @Override
    public final Integer getPID() {
        return null;
    }

    @Override
    public final Integer getExitValue() {
        return null;
    }

    public CommandToDo(final Date dateScheduled, final String stdin) {
        super(dateScheduled);
        this.stdin = stdin;
    }
}
