package io.github.greyp9.arwo.s3.connection;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.util.PropertiesX;
import software.amazon.awssdk.services.s3.S3Client;

import java.util.Date;
import java.util.Properties;

public final class S3Connection {
    private final S3Client s3Client;
    private final long dateOpen;
    private final Properties properties;
    private final PropertiesX propertiesX;

    public S3Client getS3Client() {
        return s3Client;
    }

    public Date getDateOpen() {
        return new Date(dateOpen);
    }

    public Properties getProperties() {
        return properties;
    }

    public S3Connection(final S3Client s3Client) {
        this.s3Client = s3Client;
        this.dateOpen = new Date().getTime();
        this.properties = new Properties();
        this.propertiesX = new PropertiesX(properties);
        propertiesX.setLong(App.Connection.DATE_LAST, dateOpen);
        propertiesX.setLong(App.Connection.COUNT, 0L);
        propertiesX.setLong(App.Connection.MILLIS, 0L);
    }

    public Date getDateLast() {
        return new Date(propertiesX.getLong(App.Connection.DATE_LAST));
    }

    public long getCount() {
        return propertiesX.getLong(App.Connection.COUNT);
    }

    public long getMillis() {
        return propertiesX.getLong(App.Connection.MILLIS);
    }

    public void update(final Date date) {
        // propertiesX.setLong(App.Connection.DATE_LAST, date.getTime());  // enable connection refresh via timeout
        propertiesX.addLong(App.Connection.COUNT, 1L);
        final long millis = SystemU.currentTimeMillis() - date.getTime();
        propertiesX.addLong(App.Connection.MILLIS, millis);
    }
}
