package io.github.greyp9.arwo.core.alert;

import io.github.greyp9.arwo.core.alert.action.AlertActions;
import io.github.greyp9.arwo.core.vm.mutex.CollectionU;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

public class Alerts {
    private final Collection<Alert> alertsTransient;
    private final Collection<Alert> alertsPersistent;

    public Alerts() {
        this.alertsTransient = new ArrayList<Alert>();
        this.alertsPersistent = new ArrayList<Alert>();
    }

    public final Collection<Alert> getTransient() {
        synchronized (this) {
            return CollectionU.move(new ArrayList<Alert>(), alertsTransient);
        }
    }

    public final Collection<Alert> getPersistent() {
        synchronized (this) {
            return CollectionU.copy(new ArrayList<Alert>(), alertsPersistent);
        }
    }

    public final void add(final Alert alert) {
        synchronized (this) {
            final boolean isTransient = (alert.getActions() == null);
            final Collection<Alert> alertCollection = (isTransient ? alertsTransient : alertsPersistent);
            alertCollection.add(alert);
        }
    }

    public final void remove(final String id) {
        synchronized (this) {
            final Iterator<Alert> iterator = alertsPersistent.iterator();
            while (iterator.hasNext()) {
                final Alert alert = iterator.next();
                final AlertActions actions = alert.getActions();
                if ((actions != null) && (id.equals(actions.getID()))) {
                    iterator.remove();
                }
            }
        }
    }
}
