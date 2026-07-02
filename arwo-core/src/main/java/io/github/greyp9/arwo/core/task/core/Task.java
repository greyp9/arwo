package io.github.greyp9.arwo.core.task.core;

import java.util.Date;
import java.util.concurrent.Future;

public abstract class Task {
    private final String name;
    private final Date dateInvoke;

    private Date dateStart;
    private Date dateFinish;

    private Future<?> future;

    public Task(final String name, final Date dateInvoke) {
        this.name = name;
        this.dateInvoke = dateInvoke;
    }

    public final String getName() {
        return name;
    }

    public final Date getDateInvoke() {
        return dateInvoke;
    }

    public final Date getDateStart() {
        return dateStart;
    }

    public final void setDateStart(final Date dateStart) {
        this.dateStart = dateStart;
    }

    public final Date getDateFinish() {
        return dateFinish;
    }

    public final void setDateFinish(final Date dateFinish) {
        this.dateFinish = dateFinish;
    }

    public final Future<?> getFuture() {
        return future;
    }

    public final void setFuture(final Future<?> future) {
        this.future = future;
    }

    public abstract Runnable createRunnable();
}
