package io.github.greyp9.arwo.core.logging;

import io.github.greyp9.arwo.core.date.DateX;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;

public class FormatterX extends Formatter {
    private final DateX dateX;

    public FormatterX() {
        super();
        this.dateX = DateX.Factory.createXsdUtcMilli();
    }

    public final DateX getDateX() {
        return dateX;
    }

    public final String format(final LogRecord record) {
        final Throwable thrown = record.getThrown();
        return String.format("%s [%d-%s] %-5s %s.%s - %s%n",
                dateX.toString(new Date(record.getMillis())),
                record.getThreadID(), Thread.currentThread().getName(),  // NOPMD
                record.getLevel().toString(),  // NOPMD
                record.getSourceClassName(),
                record.getSourceMethodName(),
                ((thrown == null) ? record.getMessage() : formatThrowable(thrown)));
    }

    private String formatThrowable(final Throwable thrown) {
        final StringWriter writer = new StringWriter();
        thrown.printStackTrace(new PrintWriter(writer));
        return writer.toString();
    }
}
