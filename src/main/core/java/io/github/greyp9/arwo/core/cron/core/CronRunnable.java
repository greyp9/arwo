package io.github.greyp9.arwo.core.cron.core;

@SuppressWarnings({ "PMD.AbstractNaming", "PMD.DoNotUseThreads" })
public abstract class CronRunnable implements Runnable {
    private final CronParams params;

    public final CronParams getParams() {
        return params;
    }

    protected CronRunnable(final CronParams params) {
        this.params = params;
    }
}
