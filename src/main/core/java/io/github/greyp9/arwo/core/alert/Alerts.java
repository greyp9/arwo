package io.github.greyp9.arwo.core.alert;

import java.util.ArrayList;
import java.util.Collection;

public class Alerts {
    private final Collection<Alert> values;

    public final Collection<Alert> getAlerts() {
        synchronized (this) {
            return values;
        }
    }

    public Alerts() {
        this.values = new ArrayList<Alert>();
    }

    public final void add(final Alert alert) {
        synchronized (this) {
            values.add(alert);
        }
    }

    public final void remove(final Alert alert) {
        synchronized (this) {
            values.remove(alert);
        }
    }

    public final Collection<Alert> removeAll() {
        synchronized (this) {
            final Collection<Alert> valuesLocal = new ArrayList<Alert>(values);
            values.clear();
            return valuesLocal;
        }
    }
}
