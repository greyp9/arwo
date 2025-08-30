package io.github.greyp9.arwo.s3.connection;

import io.github.greyp9.arwo.core.connect.ConnectionResource;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.lang.CompareU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;
import java.util.Date;

public final class S3ConnectionResource implements ConnectionResource, Comparable<ConnectionResource> {
    private final String region;
    private final String bucket;
    private final S3Connection connection;

    public S3ConnectionResource(final String region, final String bucket, final S3Connection connection) {
        this.region = region;
        this.bucket = bucket;
        this.connection = connection;
    }

    public S3Connection getConnection() {
        return connection;
    }

    @Override
    public String getName() {
        return Value.join(Http.Token.SLASH, region, bucket);
    }

    @Override
    public String getID() {
        return Integer.toHexString(connection.hashCode());
    }

    @Override
    public Date getDateOpen() {
        return connection.getDateOpen();
    }

    @Override
    public Date getDateLast() {
        return connection.getDateLast();
    }

    @Override
    public String getTimeout() {
        return null;
    }

    @Override
    public long getCount() {
        return connection.getCount();
    }

    @Override
    public long getMillis() {
        return connection.getMillis();
    }

    @Override
    public void close() throws IOException {
        connection.getS3Client().close();
    }

    @Override
    public int compareTo(final ConnectionResource resource) {
        return CompareU.compare(getName(), resource.getName());
    }
}
