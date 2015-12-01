package io.github.greyp9.arwo.core.util;

import java.util.Properties;

public class PropertiesX {
    private final Properties properties;

    public PropertiesX(final Properties properties) {
        this.properties = properties;
    }

    public final long getLong(final String key) {
        return getLong(key, 0L);
    }

    private long getLong(final String key, final long defaultValue) {
        final String value = properties.getProperty(key);
        return ((value == null) ? defaultValue : Long.parseLong(value));
    }

    public final long setLong(final String key, final long value) {
        properties.setProperty(key, Long.toString(value));
        return value;
    }

    public final long addLong(final String key, final long value) {
        return setLong(key, getLong(key) + value);
    }
}
