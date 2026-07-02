package io.github.greyp9.arwo.core.task.type.sleep;

import io.github.greyp9.arwo.core.task.core.Task;

import java.util.Date;

public class SleepTask extends Task {
    private final String label;
    private final String duration;

    private boolean interrupted;

    public SleepTask(final String name, final Date dateInvoke, final String label, final String duration) {
        super(name, dateInvoke);
        this.label = label;
        this.duration = duration;
    }

    public final String getLabel() {
        return label;
    }

    public final String getDuration() {
        return duration;
    }

    public final boolean isInterrupted() {
        return interrupted;
    }

    public final void setInterrupted(final boolean interrupted) {
        this.interrupted = interrupted;
    }

    @Override
    public final Runnable createRunnable() {
        return new SleepRunnable(this);
    }
}
