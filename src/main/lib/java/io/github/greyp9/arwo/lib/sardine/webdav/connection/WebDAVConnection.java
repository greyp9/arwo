package io.github.greyp9.arwo.lib.sardine.webdav.connection;

import com.github.sardine.Sardine;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.util.PropertiesX;

import java.io.IOException;
import java.net.URL;
import java.util.Date;
import java.util.Properties;

public class WebDAVConnection {
    private final Sardine connection;
    private final URL url;
    private final long dateOpen;


    private final Properties properties;
    private final PropertiesX propertiesX;

    public final Sardine getConnection() {
        return connection;
    }

    public final URL getURL() {
        return url;
    }

    public final Date getDateOpen() {
        return new Date(dateOpen);
    }


    public final Properties getProperties() {
        return properties;
    }

    public WebDAVConnection(final Sardine connection, final URL url) {
        this.connection = connection;
        this.url = url;
        this.dateOpen = new Date().getTime();


        this.properties = new Properties();
        this.propertiesX = new PropertiesX(properties);
        propertiesX.setLong("dateLast", dateOpen);
        propertiesX.setLong("count", 0L);
        propertiesX.setLong("millis", 0L);
    }

    public final Date getDateLast() {
        return new Date(propertiesX.getLong("dateLast"));
    }

    public final long getCount() {
        return propertiesX.getLong("count");
    }

    public final long getMillis() {
        return propertiesX.getLong("millis");
    }

    public final void update(final Date date) {
        propertiesX.setLong("dateLast", date.getTime());
        propertiesX.addLong("count", 1L);
        final long millis = SystemU.currentTimeMillis() - date.getTime();
        propertiesX.addLong("millis", millis);
    }

    public final void close() {
        try {
            connection.shutdown();
        } catch (IOException e) {
            properties.setProperty(e.getClass().getName(), e.getMessage());
        }
    }
}