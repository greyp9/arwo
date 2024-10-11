package io.github.greyp9.arwo.core.logging;

import io.github.greyp9.arwo.core.date.DateX;

import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;

public class FormatterXS extends Formatter {
    private final DateX dateX;

    public FormatterXS() {
        super();
        this.dateX = DateX.Factory.createXsdUtcMilli();
    }

    public final DateX getDateX() {
        return dateX;
    }

    public final String format(final LogRecord record) {
        return String.format("%s - %s%n", dateX.toString(new Date(record.getMillis())), record.getMessage());
    }
}
