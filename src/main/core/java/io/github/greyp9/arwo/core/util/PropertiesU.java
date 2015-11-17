package io.github.greyp9.arwo.core.util;

import io.github.greyp9.arwo.core.io.StreamU;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Pattern;

public final class PropertiesU {

    private PropertiesU() {
    }

    public static Properties loadFromXml(final URL url) throws IOException {
        return loadFromXml(StreamU.read(url));
    }

    public static Properties loadFromXml(final byte[] xml) throws IOException {
        final Properties properties = new Properties();
        properties.loadFromXML(new ByteArrayInputStream(xml));
        return properties;
    }

    public static void toggleBoolean(final Properties properties, final String key) {
        final boolean value = Boolean.parseBoolean(properties.getProperty(key));
        properties.setProperty(key, Boolean.toString(!value));
    }

    public static void setProperty(final Properties properties, final Object key, final Object value) {
        if (value == null) {
            properties.remove(key);
        } else {
            properties.put(key, value);
        }
    }

    public static Properties filter(final Properties properties, final String regexKey) {
        final Properties propertiesFilter = new Properties();
        final Pattern pattern = Pattern.compile(regexKey);
        for (final Map.Entry<Object, Object> entry : properties.entrySet()) {
            if (pattern.matcher(entry.getKey().toString()).matches()) {
                propertiesFilter.put(entry.getKey(), entry.getValue());
            }
        }
        return propertiesFilter;
    }

    public static Collection<String> values(final Properties properties) {
        final Collection<String> values = new ArrayList<String>();
        for (final Map.Entry<Object, Object> entry : properties.entrySet()) {
            values.add(entry.getValue().toString());
        }
        return values;
    }
}
