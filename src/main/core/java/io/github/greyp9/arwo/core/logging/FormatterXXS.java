package io.github.greyp9.arwo.core.logging;

import java.util.logging.Formatter;
import java.util.logging.LogRecord;

@SuppressWarnings("PMD")
public class FormatterXXS extends Formatter {

    public final String format(final LogRecord record) {
        return String.format("%s%n", record.getMessage());
    }
}
