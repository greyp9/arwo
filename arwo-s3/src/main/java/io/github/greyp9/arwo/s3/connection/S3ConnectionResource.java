package io.github.greyp9.arwo.s3.connection;

import io.github.greyp9.arwo.core.connect.ConnectionResource;

import java.io.IOException;
import java.util.Date;

public final class S3ConnectionResource implements ConnectionResource, Comparable<ConnectionResource> {
    private final S3Connection s3Connection;

    public S3ConnectionResource(final S3Connection s3Connection) {
        this.s3Connection = s3Connection;
    }

    public S3Connection getConnection() {
        return s3Connection;
    }

    @Override
    public String getName() {
        return "";
    }

    @Override
    public String getID() {
        return "";
    }

    @Override
    public Date getDateOpen() {
        return null;
    }

    @Override
    public Date getDateLast() {
        return null;
    }

    @Override
    public String getTimeout() {
        return "";
    }

    @Override
    public long getCount() {
        return 0;
    }

    @Override
    public long getMillis() {
        return 0;
    }

    @Override
    public void close() throws IOException {
    }

    @Override
    public int compareTo(final ConnectionResource o) {
        return 0;
    }
}
