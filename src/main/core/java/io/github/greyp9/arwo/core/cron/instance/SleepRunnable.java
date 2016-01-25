package io.github.greyp9.arwo.core.cron.instance;

import io.github.greyp9.arwo.core.cron.core.CronParams;
import io.github.greyp9.arwo.core.cron.core.CronRunnable;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.table.type.RowTyped;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

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
        logger.entering(getClass().getName(), CronRunnable.class.getName());
        final RowTyped row = getParams().getRow();
        final Date dateStart = new Date();
        row.update(Const.DATE_START, dateStart);

        final Element element = getParams().getCronJob().getElement();
        final String duration = ElementU.getAttribute(element, Const.DURATION, DurationU.Const.ZERO_SECONDS);
        final Date dateUntil = DurationU.add(dateStart, DateU.Const.TZ_GMT, duration);
        final boolean interrupted = ThreadU.sleepUntil(dateUntil);

        row.update(Const.DURATION, DurationU.duration(dateStart, new Date()));
        row.update(Const.RESULT, 0);  // i18n internal
        logger.exiting(getClass().getName(), CronRunnable.class.getName(), interrupted);
    }
}
