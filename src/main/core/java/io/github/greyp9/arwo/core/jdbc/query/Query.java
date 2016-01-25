package io.github.greyp9.arwo.core.jdbc.query;

import io.github.greyp9.arwo.core.date.Interval;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.util.PropertiesX;

import java.util.Date;
import java.util.Properties;

@SuppressWarnings("PMD.AvoidSynchronizedAtMethodLevel")
public class Query {
    private final String context;
    private final long date;
    private final String text;
    private final Properties properties;
    private final Results results;
    private Exception exception;

    public final String getContext() {
        return context;
    }

    public final Date getDate() {
        return new Date(date);
    }

    public final String getText() {
        return text;
    }

    public final Properties getProperties() {
        return properties;
    }

    public final Results getResults() {
        return results;
    }

    public final Exception getException() {
        return exception;
    }

    public Query(final String context, final long date, final String text) {
        this.context = context;
        this.date = date;
        this.text = text;
        this.properties = new Properties();
        this.results = new Results(text, new Interval(null, null));
        this.exception = null;
    }

    public final Integer getExitValue() {
        return ((exception == null) ? 0 : -1);
    }

    public final synchronized void start() {
        new PropertiesX(properties).setLong(Const.START, new Date().getTime());
    }

    public final synchronized void finish(final Exception exceptionIn) {
        new PropertiesX(properties).setLong(Const.FINISH, new Date().getTime());
        exception = exceptionIn;
    }

    private static class Const {
        private static final String START = "start";  // i18n internal
        private static final String FINISH = "finish";  // i18n internal
    }
}
