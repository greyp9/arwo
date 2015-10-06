package io.github.greyp9.arwo.core.command;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.Interval;

import java.util.Date;

public class Command {
    private final String context;
    private final String stdin;
    private final String stdout;
    private final String stderr;
    private final Date dateStart;
    private final Date dateFinish;
    private final Integer processId;
    private final Integer exitValue;

    public Command(final String context, final String stdin) {
        this(context, stdin, null, null, new Interval(null, null), null, null);
    }

    public Command(final String context, final String stdin, final Date dateStart) {
        this(context, stdin, null, null, new Interval(dateStart, null), null, null);
    }

    public Command(final String context, final String stdin, final String stdout, final String stderr,
                   final Interval interval, final Integer processId, final Integer exitValue) {
        this.context = context;
        this.stdin = stdin;
        this.stdout = stdout;
        this.stderr = stderr;
        this.dateStart = interval.getDateStart();
        this.dateFinish = interval.getDateFinish();
        this.processId = processId;
        this.exitValue = exitValue;
    }

    public final String getContext() {
        return context;
    }

    public final String getStdin() {
        return stdin;
    }

    public final String getStdout() {
        return stdout;
    }

    public final String getStderr() {
        return stderr;
    }

    public final Date getDateStart() {
        return DateU.copy(dateStart);
    }

    public final Date getDateFinish() {
        return DateU.copy(dateFinish);
    }

    public final Long getElapsed() {
        final Date dateA = dateStart;
        final Date dateZ = ((dateFinish == null) ? new Date() : dateFinish);
        return ((dateA == null) ? null : (dateZ.getTime() - dateA.getTime()));  // NOPMD
    }

    public final Integer getProcessId() {
        return processId;
    }

    public final Integer getExitValue() {
        return exitValue;
    }
}
