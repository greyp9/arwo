package io.github.greyp9.arwo.core.metric.histogram.cron;

import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogram;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogramSerializerFS;
import io.github.greyp9.arwo.core.naming.AppNaming;

import javax.naming.Binding;
import javax.naming.Context;
import java.util.Collection;
import java.util.Date;
import java.util.logging.Logger;

@SuppressWarnings("unused")
public final class TimeHistogramSaveRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final String name;
    private final Date date;
    private final String contextName;

    public TimeHistogramSaveRunnable(final String name, final Date date, final String... params) {
        this(name, date, params[0]);
    }

    public TimeHistogramSaveRunnable(final String name, final Date date, final String contextName) {
        this.name = name;
        this.date = date;
        this.contextName = contextName;
    }

    @Override
    public void run() {
        final String className = getClass().getName();
        final String methodName = "run()";
        logger.entering(className, methodName);
        logger.finer(String.format("[%s][%s][%s]", name, date.toString(), contextName));
        try {
            final Context context = AppNaming.lookupSubcontext(contextName);
            final Collection<Binding> bindings = AppNaming.listBindings(context, ".*");
            for (final Binding binding : bindings) {
                final Object object = binding.getObject();
                if (object instanceof TimeHistogram) {
                    save((TimeHistogram) object);
                }
            }
        } catch (Exception e) {
            logger.severe(e.getMessage());
        }
        logger.exiting(className, methodName);
    }

    private void save(final TimeHistogram histogram) {
        histogram.expireCache(date);
        new TimeHistogramSerializerFS().save(histogram, date);
    }
}
