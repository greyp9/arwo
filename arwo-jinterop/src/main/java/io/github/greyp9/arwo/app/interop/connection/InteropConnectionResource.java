package io.github.greyp9.arwo.app.interop.connection;

import io.github.greyp9.arwo.core.connect.ConnectionResource;
import io.github.greyp9.arwo.core.lang.CompareU;
import io.github.greyp9.arwo.lib.interop.dcom.connection.InteropConnection;

import java.util.Date;
import java.util.logging.Logger;

public class InteropConnectionResource implements ConnectionResource, Comparable<ConnectionResource> {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final String name;
    private final InteropConnection connection;

    public final InteropConnection getConnection() {
        return connection;
    }

    public InteropConnectionResource(final String name, final InteropConnection connection) {
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
        logger.finest("close()");
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
