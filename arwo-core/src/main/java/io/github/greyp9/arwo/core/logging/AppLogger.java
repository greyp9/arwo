package io.github.greyp9.arwo.core.logging;

import io.github.greyp9.arwo.core.io.StreamU;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.util.logging.LogManager;

/**
 * Provide default logging configuration (in "conf/" folder of application), while allowing override via system
 * property.
 */
public class AppLogger {

    public final void initialize() throws IOException {
        final String pathLoggingProperties = System.getProperty(SYSPROP_KEY, SYSPROP_VALUE_DEFAULT);
        final File fileLoggingProperties = new File(pathLoggingProperties);
        if (fileLoggingProperties.exists()) {
            final byte[] bytes = StreamU.read(fileLoggingProperties);
            LogManager.getLogManager().readConfiguration(new ByteArrayInputStream(bytes));
        }
    }

    private static final String SYSPROP_KEY = "java.util.logging.config.file";
    private static final String SYSPROP_VALUE_DEFAULT = "conf/logging.properties";
}
