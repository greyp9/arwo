package io.github.greyp9.arwo.app.ssh.connection;

import io.github.greyp9.arwo.core.connect.ConnectionResource;
import io.github.greyp9.arwo.core.lang.CompareU;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;

import java.util.Date;

public class SSHConnectionResource implements ConnectionResource, Comparable<ConnectionResource> {
    private final String name;
    private final SSHConnection connection;

    public final SSHConnection getConnection() {
        return connection;
    }

    public SSHConnectionResource(final String name, final SSHConnection connection) {
        this.name = name;
        this.connection = connection;
    }

    @Override
    public final String getName() {
        return name;
    }

    @Override
    public final Date getDate() {
        return connection.getDateLast();
    }

    @Override
    public final void close() {
        connection.getConnection().close();
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
