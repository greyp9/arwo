package io.github.greyp9.arwo.core.logging;

import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Logger;

public final class LoggerU {

    private LoggerU() {
    }

    @SuppressWarnings("PMD")
    public static void setFormatter(final Logger logger, final Formatter formatter) {
        Logger logger1 = logger;
        while (logger1 != null) {
            final Handler[] handlers = logger.getHandlers();
            if (handlers.length == 0) {
                logger1 = logger1.getParent();
            } else {
                for (final Handler handler : handlers) {
                    handler.setFormatter(formatter);
                }
                break;
            }
        }
    }

    public static void adjust(final Logger logger) {
        setFormatter(logger, new FormatterX());
    }

    public static void adjustShort(final Logger logger) {
        setFormatter(logger, new FormatterXS());
    }

    public static void adjustTiny(final Logger logger) {
        setFormatter(logger, new FormatterXXS());
    }
}
