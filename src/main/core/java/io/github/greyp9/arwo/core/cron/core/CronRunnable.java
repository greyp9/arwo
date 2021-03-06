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

    public static class Const {
        public static final String CRON = "cron";  // i18n internal
        public static final String DATE_START = "dateStart";  // i18n internal
        public static final String DURATION = "duration";  // i18n internal
        public static final String RESULT = "result";  // i18n internal
    }
}
