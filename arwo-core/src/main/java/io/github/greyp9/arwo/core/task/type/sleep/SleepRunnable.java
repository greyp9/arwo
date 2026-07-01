package io.github.greyp9.arwo.core.task.type.sleep;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;

import java.util.Date;
import java.util.logging.Logger;

public class SleepRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final SleepTask task;

    public SleepRunnable(final SleepTask task) {
        this.task = task;
    }

    @Override
    public final void run() {
        logger.entering(getClass().getName(), "run()");
        task.setDateStart(new Date());
        final Date dateUntil = DurationU.add(new Date(), DateU.Const.TZ_GMT, task.getDuration());
        task.setInterrupted(ThreadU.sleepUntil(dateUntil));
        task.setDateFinish(new Date());
        logger.exiting(getClass().getName(), "run()");
    }
}
