package io.github.greyp9.arwo.core.connect;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.mutex.CollectionU;

import java.io.IOException;
import java.util.Collection;
import java.util.TreeSet;

@SuppressWarnings("PMD.AvoidSynchronizedAtMethodLevel")
public class ConnectionCache {
    private final String cacheName;
    private final Collection<ConnectionResource> resources;
    private final Alerts alerts;

    public ConnectionCache(final String cacheName, final Alerts alerts) {
        this.cacheName = cacheName;
        this.resources = new TreeSet<ConnectionResource>();
        this.alerts = alerts;
    }

    public final synchronized Collection<ConnectionResource> getResources() {
        return CollectionU.copy(new TreeSet<ConnectionResource>(), resources);
    }

    public final synchronized ConnectionResource getResource(
            final String name, final ConnectionFactory factory) throws IOException {
        ConnectionResource resource = findResource(name);
        if ((resource == null) && (factory != null)) {
            resource = factory.create(name);
            if (resource != null) {
                alerts.add(new Alert(Alert.Severity.INFO, String.format("[+%s] %s", cacheName, name)));
                resources.add(resource);
            }
        }
        return resource;
    }

    public final synchronized ConnectionResource removeResource(final String name) throws IOException {
        final ConnectionResource resource = findResource(name);
        if (resource != null) {
            alerts.add(new Alert(Alert.Severity.INFO, String.format("[-%s] %s", cacheName, name)));
            resources.remove(resource);
            resource.close();
        }
        return resource;
    }

    private ConnectionResource findResource(final String name) {
        ConnectionResource resource = null;
        for (final ConnectionResource resourceIt : resources) {
            final String nameIt = resourceIt.getName();
            if (Value.equal(nameIt, name)) {
                resource = resourceIt;
                break;
            }
        }
        return resource;
    }
}
