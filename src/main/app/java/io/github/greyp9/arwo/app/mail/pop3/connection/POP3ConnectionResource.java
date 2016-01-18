package io.github.greyp9.arwo.app.mail.pop3.connection;

import io.github.greyp9.arwo.core.connect.ConnectionResource;
import io.github.greyp9.arwo.core.lang.CompareU;
import io.github.greyp9.arwo.lib.mail.pop3.connection.POP3Connection;

import java.util.Date;

public class POP3ConnectionResource implements ConnectionResource, Comparable<ConnectionResource> {
    private final String name;
    private final POP3Connection connection;

    public final POP3Connection getConnection() {
        return connection;
    }

    public POP3ConnectionResource(final String name, final POP3Connection connection) {
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
