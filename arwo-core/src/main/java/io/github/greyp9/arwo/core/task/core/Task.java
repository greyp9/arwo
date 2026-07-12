package io.github.greyp9.arwo.core.task.core;

import java.io.File;
import java.util.Date;
import java.util.concurrent.Future;

public abstract class Task {
    private final String name;
    private Date dateSubmit;

    private Date dateStart;
    private Date dateFinish;

    private Future<?> future;

    public Task(final String name, final Date dateSubmit) {
        this.name = name;
        this.dateSubmit = dateSubmit;
    }

    public final String getName() {
        return name;
    }

    public final Date getDateSubmit() {
        return dateSubmit;
    }

    public final void setDateSubmit(final Date dateSubmit) {
        this.dateSubmit = dateSubmit;
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

    public abstract Runnable createRunnable(File folderPersist);

    public static class Const {
        public static final String FIELD_NAME = "name";
        public static final String FIELD_DATE_FINISH = "finish";
        public static final String FIELD_DATE_START = "start";
        public static final String FIELD_DATE_SUBMIT = "submit";
    }
}
