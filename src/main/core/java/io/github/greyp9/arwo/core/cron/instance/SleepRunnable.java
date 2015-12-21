package io.github.greyp9.arwo.core.cron.instance;

import io.github.greyp9.arwo.core.cron.core.CronParams;
import io.github.greyp9.arwo.core.cron.core.CronRunnable;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;
import io.github.greyp9.arwo.core.xml.ElementU;

import java.util.Date;
import java.util.logging.Logger;

@SuppressWarnings("unused")
public class SleepRunnable extends CronRunnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public SleepRunnable(final CronParams params) {
        super(params);
    }

    @Override
    public final void run() {
        logger.info("START");
        final Date dateStart = new Date();
        final String duration = ElementU.getAttribute(getParams().getCronJob().getElement(), "duration", "PT0S");
        final Date dateUntil = DurationU.add(dateStart, DateU.Const.TZ_GMT, duration);
        final boolean interrupted = ThreadU.sleepUntil(dateUntil);
        logger.info(String.format("FINISH(interrupted=%s)", interrupted));
        //Date dateFinish = new Date();
    }
}
