package io.github.greyp9.arwo.core.util;

import io.github.greyp9.arwo.core.io.StreamU;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Properties;

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
}
