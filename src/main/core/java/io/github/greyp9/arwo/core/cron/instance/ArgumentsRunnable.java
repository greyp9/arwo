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
        final RowTyped row = getParams().getRow();
        logger.info("START");
        final Date dateStart = new Date();
        row.update("dateStart", dateStart);

        final String arguments = ElementU.getAttribute(getParams().getCronJob().getElement(), "arguments");
        final NameTypeValues httpArguments = HttpArguments.toArguments(arguments);
        for (final NameTypeValue httpArgument : httpArguments) {
            logger.info(httpArgument.toString());
        }

        logger.info("FINISH");
        final Date dateFinish = new Date();
        row.update("duration", DurationU.duration(dateStart, dateFinish));
        row.update("result", 0);
    }
}
