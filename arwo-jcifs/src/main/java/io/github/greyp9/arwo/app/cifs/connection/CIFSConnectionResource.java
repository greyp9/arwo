package io.github.greyp9.arwo.app.cifs.connection;

import io.github.greyp9.arwo.core.connect.ConnectionResource;
import io.github.greyp9.arwo.core.lang.CompareU;
import io.github.greyp9.arwo.lib.jcifs.fs.connection.CIFSConnection;

import java.util.Date;

public class CIFSConnectionResource implements ConnectionResource, Comparable<ConnectionResource> {
    private final String name;
    private final CIFSConnection connection;

    public final CIFSConnection getConnection() {
        return connection;
    }

    public CIFSConnectionResource(final String name, final CIFSConnection connection) {
        this.name = name;
        this.connection = connection;
    }

    @Override
    public final String getName() {
        return name;
    }

    @Override
    public final String getID() {
        return Integer.toHexString(connection.hashCode());
    }

    @Override
    public final Date getDateOpen() {
        return connection.getDateOpen();
    }

    @Override
    public final Date getDateLast() {
        return connection.getDateLast();
    }

    @Override
    public String getTimeout() {
        return null;
    }

    @Override
    public final long getCount() {
        return connection.getCount();
    }

    @Override
    public final long getMillis() {
        return connection.getMillis();
    }

    @Override
    public final void close() {
        connection.close();
    }

    @Override
    public final int compareTo(final ConnectionResource resource) {
        return CompareU.compare(getName(), (resource == null) ? null : resource.getName());
    }

    @Override
    public final boolean equals(final Object o) {
        return ((o instanceof ConnectionResource) && (compareTo((ConnectionResource) o) == 0));
    }

    @Override
    public final int hashCode() {
        return ((getName() == null) ? 0 : getName().hashCode());
    }
}
