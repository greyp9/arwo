package io.github.greyp9.arwo.core.cron.instance;

import io.github.greyp9.arwo.core.cron.core.CronParams;
import io.github.greyp9.arwo.core.cron.core.CronRunnable;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.table.type.RowTyped;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xml.ElementU;

import java.util.Date;
import java.util.logging.Logger;

@SuppressWarnings("unused")
public class ArgumentsRunnable extends CronRunnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public ArgumentsRunnable(final CronParams params) {
        super(params);
    }

    @Override
    public final void run() {
        logger.entering(getClass().getName(), CronRunnable.class.getName());
        final RowTyped row = getParams().getRow();
        final Date dateStart = new Date();
        row.update(Const.DATE_START, dateStart);

        final String arguments = ElementU.getAttribute(getParams().getCronJob().getElement(), "arguments");  // i18n
        final NameTypeValues httpArguments = HttpArguments.toArguments(arguments);
        for (final NameTypeValue httpArgument : httpArguments) {
            logger.finest(httpArgument.toString());
        }

        row.update(Const.DURATION, DurationU.duration(dateStart, new Date()));
        row.update(Const.RESULT, 0);  // i18n
        logger.exiting(getClass().getName(), CronRunnable.class.getName());
    }
}
