package io.github.greyp9.arwo.core.jdbc.query;

import io.github.greyp9.arwo.core.date.Interval;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.util.PropertiesX;

import java.util.Date;
import java.util.Properties;

public class Query {
    private final String context;
    private final long date;
    private final String text;
    private final Properties properties;
    private final Results results;

    public String getContext() {
        return context;
    }

    public final Date getDate() {
        return new Date(date);
    }

    public String getText() {
        return text;
    }

    public Properties getProperties() {
        return properties;
    }

    public Results getResults() {
        return results;
    }

    public Query(String context, long date, String text) {
        this.context = context;
        this.date = date;
        this.text = text;
        this.properties = new Properties();
        this.results = new Results(text, new Interval(null, null));
    }

    public final synchronized void start() {
        new PropertiesX(properties).setLong("start", new Date().getTime());
    }

    public final synchronized void finish() {
        new PropertiesX(properties).setLong("finish", new Date().getTime());
    }
}
