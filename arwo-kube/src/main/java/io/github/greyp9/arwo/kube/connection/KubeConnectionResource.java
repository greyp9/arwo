package io.github.greyp9.arwo.kube.connection;

import io.github.greyp9.arwo.core.connect.ConnectionResource;
import io.github.greyp9.arwo.core.lang.CompareU;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.util.Date;

public final class KubeConnectionResource implements ConnectionResource, Comparable<ConnectionResource> {
    private final String name;
    private final String namespace;
    private final KubeConnection connection;

    public KubeConnectionResource(final String name, final String namespace, final KubeConnection connection) {
        this.name = name;
        this.namespace = namespace;
        this.connection = connection;
    }

    @Override
    public String getName() {
        return name;
    }

    public String getNamespace() {
        return namespace;
    }

    public KubeConnection getConnection() {
        return connection;
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
        return "PT12M";
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
        connection.getCoreV1Api().getApiClient().getHttpClient().connectionPool().evictAll();
    }

    @Override
    public int compareTo(@NotNull final ConnectionResource resource) {
        return CompareU.compare(getName(), resource.getName());
    }
}
